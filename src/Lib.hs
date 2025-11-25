{-# LANGUAGE OverloadedStrings #-}
-- Включение расширения для перегруженных строковых литералов
-- Позволяет использовать строки как значения типа Text без явного преобразования

module Lib ( -- Объявление модуля с экспортируемыми типами и функциями
    NGramModel,
    splitText,
    buildNGramModel,
    saveModel,
    loadModel,
    generateContinuation,
    getPossibleKeys,
    findValidKey,
    getRandomKey,
    preprocessText
) where

import Data.Char (isLetter, toLower, isPunctuation, isSpace, isAscii)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (tails, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (randomRIO, getStdGen, randomR, StdGen, newStdGen)
import System.IO
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)
import Data.Ord (Down(..))

type NGram = [Text] -- Определение типа для N-граммы
type NGramModel = HashMap NGram [(NGram, Double)] -- Определение типа для N-граммной модели

-- Упрощенная очистка слова
cleanWord :: Text -> Text
cleanWord word = 
    let cleaned = T.toLower $ T.filter validChar word
     -- T.toLower: приведение к нижнему регистру
     -- T.filter validChar: фильтрация только валидных символов
    in if isValidCleanedWord cleaned then cleaned else T.empty -- Проверка валидности очищенного слова
  where
    validChar c = isAscii c && (isLetter c || c == '\'' || c == '-')
    isValidCleanedWord w = not (T.null w) && T.any isLetter w
        -- isAscii: проверка что символ ASCII
        -- isLetter: проверка что символ буква
        -- разрешены также апостроф и дефис внутри слов


-- Проверка валидности слова
isValidWord :: Text -> Bool
isValidWord word = 
    not (T.null word) && T.all validChar word && T.any isLetter word
  where
    validChar c = isAscii c && (isLetter c || c == '\'' || c == '-')
    --слово не пустое и содержит хоть 1 букву

-- Функция для проверки завершения предложения
isSentenceTerminator :: Char -> Bool
isSentenceTerminator c = c `elem` (".!?" :: [Char])

-- Оптимизированное разделение текста на предложения
splitText :: Text -> [[Text]]
splitText text = 
    let preprocessed = preprocessText text
        -- Предварительная обработка текста
        sentences = splitSentences preprocessed
        -- Разбивка на предложения
    in filter (not . null) $ map processSentence sentences
  where
    -- Рекурсивная функция разбивки текста на предложения
    splitSentences :: Text -> [Text]
    splitSentences t
        | T.null t = []
        | otherwise =
            let (sentence, rest) = T.break isSentenceTerminator t -- T.break: разбивает текст на часть до разделителя и посл
                (punctuation, rest') = T.span isSentenceTerminator rest -- T.span: берет последовательность разделителей
            in if T.null sentence
               then splitSentences rest'
               else (cleanSentence sentence `T.append` T.take 1 punctuation) : splitSentences rest'
-- Добавление очищенного предложения с первым знаком препинания

-- Функция очистки предложения
    cleanSentence :: Text -> Text
    cleanSentence = removeEdgePunctuation . T.strip
      -- T.strip: удаление пробелов по краям
    -- removeEdgePunctuation: удаление знаков препинания по краям
    
    
 -- Удаление знаков препинания с начала и конца предложения
    removeEdgePunctuation :: Text -> Text
    removeEdgePunctuation s =
        let startPunct :: Text
            startPunct = " \"'`([{" -- Символы для удаления в начале
            endPunct :: Text
            endPunct = " \"'`)]}" -- Символы для удаления в конце
            removeStart = T.dropWhile (`T.elem` startPunct) s  -- Удаление начальных знаков препинания
            removeEnd = T.reverse $ T.dropWhile (`T.elem` endPunct) (T.reverse removeStart) -- Удаление конечных знаков препинания через реверс
        in removeEnd

 -- Обработка одного предложения: преобразование в список слов
    processSentence :: Text -> [Text]
    processSentence = filter isValidWord . map (T.toLower) . T.words . T.map normalizeChar
        -- T.map normalizeChar: нормализация символов
        -- T.words: разбивка на слова по пробелам
        -- map T.toLower: приведение к нижнему регистру
        -- filter isValidWord: фильтрация валидных слов

-- Функция нормализации символов
    normalizeChar :: Char -> Char
    normalizeChar c
        | c `elem` (",;:\"''`()[]{}" :: String) = ' ' -- Замена этих знаков препинания на пробелы
        | isPunctuation c && c /= '\'' && c /= '-' = ' ' -- Замена остальных знаков препинания (кроме апострофа и дефиса)
        | otherwise = c -- Сохранение остальных символов

-- Строгая проверка N-граммы
filterValidNGram :: (NGram, NGram) -> Bool
filterValidNGram (key, value) = 
    all isValidWord (key ++ value) -- all isValidWord: проверка что все слова в ключе и значении валидны

--построение N-граммной модели
buildNGramModel :: [[Text]] -> Int -> NGramModel
buildNGramModel sentences n =
    let validSentences = filter (all isValidWord) sentences
        allNGrams = concatMap (getNGrams n) validSentences -- Фильтрация предложений с только валидными словами
        grouped = HashMap.fromListWith (++) [(key, [(value, 1)]) | (key, value) <- allNGrams] -- Извлечение всех N-грамм из всех предложений
    in HashMap.map calculateProbabilities grouped -- Группировка N-грамм по ключам с подсчетом частот
  where
    getNGrams :: Int -> [Text] -> [(NGram, NGram)] -- Функция извлечения N-грамм из одного предложения
    getNGrams n words =
        [ (take k context, take (n - k) continuation)
        | k <- [1..min 3 (length words)] --Ключ
        , (context, continuation) <- zip (tails words) (drop k (tails words))
        -- tails words: все суффиксы списка слов
        -- drop k (tails words): суффиксы, начинающиеся с k-го слова
        , length context >= k -- Проверка достаточной длины контекста
        , length continuation >= n - k -- Проверка достаточной длины продолжения
        , length context + length continuation >= n -- Проверка общей достаточной длины
        , filterValidNGram (take k context, take (n - k) continuation) -- Проверка валидности N-граммы
        ]

-- Расчет вероятностей для списка пар (N-грамма, частота)
    calculateProbabilities :: [(NGram, Int)] -> [(NGram, Double)]
    calculateProbabilities pairs =
        let total = fromIntegral $ sum (map snd pairs) -- Сумма всех частот (преобразованная в Double)
        in [(ngram, fromIntegral count / total) | (ngram, count) <- pairs] -- Вычисление вероятности для каждой N-граммы

-- Предварительная обработка текста
preprocessText :: Text -> Text
preprocessText text = 
    let asciiOnly = T.filter isAscii text -- Фильтрация только ASCII символов
        cleaned = preserveWordHyphens asciiOnly  -- Сохранение дефисов внутри слов
    in T.unwords . T.words $ cleaned -- Нормализация пробелов

-- Сохраняет дефисы внутри слов
preserveWordHyphens :: Text -> Text
preserveWordHyphens = T.unwords . map processWord . T.words -- Разбивка на слова, обработка каждого, сбор обратно
  where
    processWord w
        | T.all (== '-') w = T.empty -- Удаление слов состоящих только из дефисов
        | hasValidHyphens w = w -- Сохранение слов с валидными дефисами
        | otherwise = T.filter (\c -> c /= '-' || isLetter c) w -- Удаление дефисов не между буквами
    
    hasValidHyphens word =  -- Проверка что дефисы в слове валидны
        let parts = T.splitOn "-" word --Разбивка по дефисам
        in all (T.any isLetter) parts && length parts > 1  -- Все части содержат буквы и есть хотя бы один дефис

-- Сохранение модели в файл
saveModel :: FilePath -> NGramModel -> IO ()
saveModel filePath model = withFile filePath WriteMode $ \h -> 
    mapM_ (\(k, vs) -> TIO.hPutStrLn h $ T.pack (show k) `T.append` " => " `T.append` T.pack (show vs)) (HashMap.toList model)
    -- withFile: безопасное открытие файла
    -- HashMap.toList: преобразование HashMap в список пар
    -- mapM_: выполнение IO действия для каждого элемента
    -- TIO.hPutStrLn: запись строки в файл



-- Загрузка модели из файла
loadModel :: FilePath -> IO (Maybe NGramModel)
loadModel filePath = do
    result <- try (TIO.readFile filePath) :: IO (Either SomeException Text)
    case result of -- Безопасное чтение файла с обработкой исключений
        Left _ -> return Nothing -- Ошибка чтения
        Right content -> 
            let lines' = T.lines content
                parsed = catMaybes $ map parseLine lines'  -- Разбивка содержимого на строки
            in return $ if null parsed then Nothing else Just (HashMap.fromList parsed)  -- Парсинг каждой строки, извлечение успешных результатов
  where -- Создание HashMap из распарсенных данных
  -- Функция парсинга одной строки файла модели
    parseLine :: Text -> Maybe (NGram, [(NGram, Double)]) 
    parseLine line =
        let (keyPart, valuePart) = T.break (== '=') line -- Разделение строки на ключ и значение
        in case T.uncons valuePart of
            Just ('=', rest) -> -- Найден символ '='
                case T.uncons rest of
                    Just ('>', rest') -> -- Найден символ '>'
                        case T.uncons rest' of
                            Just (' ', valueStr) -> -- Найден пробел после "=>"
                                case readMaybe (T.unpack (T.strip keyPart)) of
                                    Just key -> -- Парсинг ключа
                                        case readMaybe (T.unpack valueStr) of -- Парсинг значения
                                            Just values -> Just (key, values)
                                            Nothing -> Nothing
                                    Nothing -> Nothing
                            _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

--генерация продолжения предложения
generateContinuation :: NGramModel -> [Text] -> Int -> IO [Text]
generateContinuation model startWords maxLength = do
    gen <- newStdGen -- Создание нового генератора случайных чисел
    return $ generateEfficient model startWords gen maxLength -- Запуск эффективной генерации
  where
    -- Внутренняя функция инициализации генерации
    generateEfficient :: NGramModel -> [Text] -> StdGen -> Int -> [Text]
    generateEfficient model currentWords gen maxLength =
        generateLoop model currentWords gen maxLength currentWords
    
    -- Рекурсивный цикл генерации
    generateLoop :: NGramModel -> [Text] -> StdGen -> Int -> [Text] -> [Text]
    generateLoop _ _ _ 0 acc = acc -- Достигнута максимальная длина
    generateLoop model currentWords gen remaining acc =
        let possibleKeys = getPossibleKeys currentWords -- Получение возможных ключей из текущего контекста
            key = findValidKey model possibleKeys -- Поиск первого валидного ключа в модели
        in case key of
            Nothing -> acc  -- Не найден подходящий ключ
            Just k -> 
                case HashMap.lookup k model of
                    Nothing -> acc
                    Just continuations ->
                        let (choice, newGen) = efficientWeightedRandom continuations gen
                            newWords = acc ++ choice -- Взвешенный случайный выбор продолжения
                            newRemaining = remaining - length choice -- Добавление выбранных слов к результату
                        in if newRemaining <= 0  -- Уменьшение оставшегося лимита
                           then newWords -- Превышен лимит
                           else generateLoop model (currentWords ++ choice) newGen newRemaining newWords

    -- Эффективный взвешенный случайный выбор с предварительной сортировкой
    efficientWeightedRandom :: [(NGram, Double)] -> StdGen -> (NGram, StdGen)
    efficientWeightedRandom choices gen =
        let total = sum (map snd choices)
            (r, newGen) = randomR (0, total) gen  -- Сумма всех вероятностей
            -- Генерация случайного числа в диапазоне [0, total]
            sortedChoices = sortOn (Down . snd) choices
        in pick sortedChoices r newGen  -- Сортировка по убыванию вероятности для оптимизации
      where -- Выбор варианта на основе случайного числа
        pick [] _ g = ([], g)
        pick ((ngram, prob):rest) rVal g
            | rVal <= prob = (ngram, g)  -- Случайное число попало в интервал этого варианта
            | otherwise = pick rest (rVal - prob) g -- Переход к следующему варианту

-- Получение возможных ключей из фразы в правильном порядке
getPossibleKeys :: [Text] -> [[Text]]
getPossibleKeys words =
    let n = length words
        -- Берем последние 1, 2, 3 слова в прямом порядке
        key1 = if n >= 1 then [last words] else []
        key2 = if n >= 2 then drop (n-2) words else []
        key3 = if n >= 3 then drop (n-3) words else []
    in filter (not . null) [key3, key2, key1]  -- Сначала проверяем самые длинные ключи

-- Поиск валидного ключа в модели
findValidKey :: NGramModel -> [[Text]] -> Maybe [Text]
findValidKey model keys = 
    case filter (`HashMap.member` model) keys of
        [] -> Nothing
        (k:_) -> Just k

-- Получение случайного ключа из модели
getRandomKey :: NGramModel -> IO (Maybe [Text])
getRandomKey model = 
    if HashMap.null model 
        then return Nothing  -- Модель пуста
        else do
            let keys = HashMap.keys model
            idx <- randomRIO (0, length keys - 1)  -- Генерация случайного индекса
            return $ Just (keys !! idx) -- Возврат ключа по случайному индексу