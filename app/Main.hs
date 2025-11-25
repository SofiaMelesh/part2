{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Control.Exception (catch, IOException)
import Control.Monad (forever, when)
import Data.List (isSuffixOf)
import Text.Read (readMaybe)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Lib
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ModelWithInfo = ModelWithInfo {
    model :: NGramModel,
    maxSentenceLength :: Int
} deriving (Show)

main :: IO ()
main = do
    TIO.putStrLn "\n 4-GRAM TEXT GENERATOR"
    mainLoop Nothing Nothing -- Запуск основного цикла с пустыми моделями

mainLoop :: Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
mainLoop currentModel model2 = do
    TIO.putStrLn "\n MENU"
    TIO.putStrLn "0. Exit"
    TIO.putStrLn "1. Load or build main model"
    TIO.putStrLn "2. Save the current model to file"
    TIO.putStrLn "3. Generate the continuation of the sentence"
    TIO.putStrLn "4. Load or build second model for the dialogue"
    TIO.putStrLn "5. Dialogue between two models"
    
    -- Отображение текущих моделей
    TIO.putStrLn $ T.pack $ "\n Current first model: " ++ modelStatus currentModel
    TIO.putStrLn $ T.pack $ "Current second model: " ++ modelStatus model2
    
    choice <- TIO.getLine
    case T.unpack choice of
            
        "1" -> do
            TIO.putStrLn "\n Loading or building main model"
            TIO.putStrLn "1. Build from a text file"
            TIO.putStrLn "2. Load from model file"
            TIO.putStrLn "0. Return"
            subChoice <- TIO.getLine
            case T.unpack subChoice of
                "0" -> mainLoop currentModel model2
                "1" -> do
                    loadTextModelLoop currentModel model2 -- Построение модели из текста
                "2" -> do
                    loadModelLoop currentModel model2  -- Загрузка модели из файла
                _ -> do
                    TIO.putStrLn "Wrong choice!"
                    mainLoop currentModel model2
            
        "2" -> do
            case currentModel of
                Nothing -> do -- Если модель не загружена
                    TIO.putStrLn "Build or download the model first!"
                    mainLoop currentModel model2
                Just modelInfo -> do -- Если модель есть
                    TIO.putStrLn "Enter a file name to save (0 to return):"
                    fileName <- TIO.getLine
                    when (fileName /= "0") $ do -- Если не введен 0
                        saveModel (T.unpack fileName) (model modelInfo) -- Сохранение модели
                        TIO.putStrLn "Model saved!"
                        mainLoop currentModel model2
                    when (fileName == "0") $ mainLoop currentModel model2
                    
        "3" -> do
            case currentModel of
                Nothing -> do -- Проверка наличия модели
                    TIO.putStrLn "Build or download the model first!"
                    mainLoop currentModel model2
                Just modelInfo -> do
                    TIO.putStrLn "Enter starting words (1-3 words, 0 to return):"
                    input <- TIO.getLine
                    when (input /= "0") $ do
                        let wordsList = map T.toLower (T.words input) -- Разбивка на слова и приведение к нижнему регистру
                        if length wordsList >= 1 && length wordsList <= 3 -- Проверка количества слов
                            then do
                                let initialLen = length wordsList
                                let maxTotalLen = maxSentenceLength modelInfo
                                
                                if maxTotalLen <= initialLen  -- Проверка возможности генерации (не превышает ли начальная длина максимальную)
                                    then do
                                        TIO.putStrLn $ T.pack $ "Initial words already have " ++ show initialLen ++ 
                                            " words, but maximum sentence length is " ++ show maxTotalLen ++ 
                                            ". Cannot generate continuation."
                                        mainLoop currentModel model2
                                    else do

                                        -- Минимум 1 дополнительное слово, максимум - оставшееся место
                                        let maxAdditional = maxTotalLen - initialLen
                                        -- Генерация случайной длины продолжения
                                        randomAdditionalLen <- if maxAdditional > 0 
                                            then randomRIO (1, maxAdditional)
                                            else return 0
                                        
                                        if randomAdditionalLen <= 0
                                            then do
                                                TIO.putStrLn "Cannot generate continuation - no space for additional words."
                                                mainLoop currentModel model2
                                            else do
                                                -- Генерация продолжения с помощью модели
                                                result <- generateContinuation (model modelInfo) wordsList randomAdditionalLen
                                                let totalLength = length result
                                                TIO.putStrLn $ "Generated continuation: " `T.append` T.unwords result
                                                mainLoop currentModel model2
                            else do
                                TIO.putStrLn "Error: Please enter between 1 and 3 words."
                                mainLoop currentModel model2
                    when (input == "0") $ mainLoop currentModel model2
            
        "4" -> do
            TIO.putStrLn "\n Loading the second model"
            TIO.putStrLn "1. Build from a text file"
            TIO.putStrLn "2. Load from model file"
            TIO.putStrLn "0. Return"
            subChoice <- TIO.getLine
            case T.unpack subChoice of
                "0" -> mainLoop currentModel model2
                "1" -> do
                    loadTextModelForSecondLoop currentModel model2
                "2" -> do
                    loadSecondModelLoop currentModel model2
                _ -> do
                    TIO.putStrLn "Wrong choice!"
                    mainLoop currentModel model2
            
        "5" -> do
            case (currentModel, model2) of
                (Just m1, Just m2) -> do -- Проверка наличия обеих моделей
                    TIO.putStrLn "Select model order: "
                    TIO.putStrLn "1. The main model starts first"
                    TIO.putStrLn "2. The second model starts first"
                    TIO.putStrLn "0. Return"
                    orderChoice <- TIO.getLine
                    case T.unpack orderChoice of
                        "0" -> mainLoop currentModel model2
                        "1" -> startDialogLoop (model m1) (model m2) currentModel model2 -- Первая модель начинает
                        "2" -> startDialogLoop (model m2) (model m1) currentModel model2 -- Вторая
                        _ -> do
                            TIO.putStrLn "Wrong choice!"
                            mainLoop currentModel model2
                _ -> do --если не хватает моделей
                    TIO.putStrLn "First, download both models! (the main and the second)"
                    mainLoop currentModel model2
            
        "0" -> TIO.putStrLn "Exit..."
        _ -> do
            TIO.putStrLn "Wrong choice!"
            mainLoop currentModel model2

--функция для статуса модели
modelStatus :: Maybe ModelWithInfo -> String
modelStatus Nothing = "not loaded"
modelStatus (Just _) = "loaded"

--функция для запуска диалога
startDialogLoop :: NGramModel -> NGramModel -> Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
startDialogLoop firstModel secondModel currentModel model2 = do
    TIO.putStrLn "Enter the initial word (0 to return):"
    startWord <- TIO.getLine
    case startWord of
        "0" -> mainLoop currentModel model2
        _ -> do
            let wordsList = T.words startWord -- Разбивка введенного текста на слова
            if length wordsList /= 1 --проверка, что введено ровно одно слово
                then do
                    TIO.putStrLn "Error: Please enter exactly one word. Try again or enter 0 to return."
                    startDialogLoop firstModel secondModel currentModel model2 -- Повторный запрос
                else do
                    TIO.putStrLn "Enter the number of replicas (0 to return):"
                    repsStr <- TIO.getLine
                    case repsStr of
                        "0" -> mainLoop currentModel model2
                        _ -> do
                            let reps = safeRead (T.unpack repsStr) 5 -- Безопасное чтение числа реплик
                            startDialog firstModel secondModel wordsList reps -- Запуск диалога
                            mainLoop currentModel model2

--цикл загрузки модели из файла с повторением при ошибке
loadModelLoop :: Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
loadModelLoop currentModel model2 = do
    TIO.putStrLn "Enter the file name of the model (0 to return):"
    fileName <- TIO.getLine
    case fileName of
        "0" -> mainLoop currentModel model2
        _ -> do
            loadedModel <- loadModel (T.unpack fileName) -- Загрузка модели из файла
            case loadedModel of
                Just m -> do
                    TIO.putStrLn "Model loaded!"
                    -- Создание ModelWithInfo с моделью и дефолтной длиной предложения 20
                    mainLoop (Just $ ModelWithInfo m 20) model2
                Nothing -> do
                    TIO.putStrLn "Model loading error! Please try again."
                    loadModelLoop currentModel model2 -- Повтор при ошибке

--цикл загрузки модели из текстового файла
loadTextModelLoop :: Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
loadTextModelLoop currentModel model2 = do
    TIO.putStrLn "Enter a file name with text (0 to return):"
    fileName <- TIO.getLine
    case fileName of
        "0" -> mainLoop currentModel model2
        _ -> do
            result <- readFileSafe (T.unpack fileName) -- Безопасное чтение файла
            case result of
                Left errorMsg -> do -- Обработка ошибки чтения
                    TIO.putStrLn $ T.pack errorMsg
                    loadTextModelLoop currentModel model2  -- Повтор при ошибке
                Right content -> do
                    let sentences = splitText (T.pack content) -- Разбивка текста на предложения
                    -- Подсчет и вывод количества слов и максимальной длины предложения
                    let wordCount = sum (map length sentences)
                    let maxSentenceLen = if null sentences then 0 else maximum (map length sentences)
                    TIO.putStrLn $ "Text loaded. Total words: " `T.append` T.pack (show wordCount)
                    TIO.putStrLn $ "Max sentence length: " `T.append` T.pack (show maxSentenceLen)
                    let newModel = buildNGramModel sentences 4
                    TIO.putStrLn "Model successfully built!"
                    -- Сохраняем модель и максимальную длину предложения
                    mainLoop (Just $ ModelWithInfo newModel maxSentenceLen) model2

-- Цикл загрузки второй модели из текстового файла
loadTextModelForSecondLoop :: Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
loadTextModelForSecondLoop currentModel model2 = do
    TIO.putStrLn "Enter a file name with text (0 to return): "
    fileName <- TIO.getLine
    case fileName of
        "0" -> mainLoop currentModel model2
        _ -> do
            result <- readFileSafe (T.unpack fileName)
            case result of
                Left errorMsg -> do
                    TIO.putStrLn $ T.pack errorMsg
                    loadTextModelForSecondLoop currentModel model2
                Right content -> do
                    let sentences = splitText (T.pack content)
                    -- Подсчет и вывод количества слов и максимальной длины
                    let wordCount = sum (map length sentences)
                    let maxSentenceLen = if null sentences then 0 else maximum (map length sentences)
                    TIO.putStrLn $ "Text loaded. Total words: " `T.append` T.pack (show wordCount)
                    TIO.putStrLn $ "Max sentence length: " `T.append` T.pack (show maxSentenceLen)
                    let newModel = buildNGramModel sentences 4
                    TIO.putStrLn "The second model has been successfully built!"
                    mainLoop currentModel (Just $ ModelWithInfo newModel maxSentenceLen)

-- Цикл загрузки второй модели из файла модели
loadSecondModelLoop :: Maybe ModelWithInfo -> Maybe ModelWithInfo -> IO ()
loadSecondModelLoop currentModel model2 = do
    TIO.putStrLn "Enter the file name of the model (0 to return): "
    fileName <- TIO.getLine
    case fileName of
        "0" -> mainLoop currentModel model2
        _ -> do
            loadedModel <- loadModel (T.unpack fileName)
            case loadedModel of
                Just m -> do
                    TIO.putStrLn "The second model has been loaded!"
                    -- Для загруженных моделей устанавливаем дефолтную максимальную длину
                    mainLoop currentModel (Just $ ModelWithInfo m 20)
                Nothing -> do
                    TIO.putStrLn "Model loading error! Please try again."
                    loadSecondModelLoop currentModel model2

-- Функция начала диалога между двумя моделями
startDialog :: NGramModel -> NGramModel -> [Text] -> Int -> IO ()
startDialog model1 model2 startWords reps = do
    TIO.putStrLn "\n START OF DIALOGUE"
    TIO.putStrLn $ "Starting word: " `T.append` T.unwords startWords
    dialogLoop model1 model2 startWords reps 0

-- Основной цикл диалога между моделями
dialogLoop :: NGramModel -> NGramModel -> [Text] -> Int -> Int -> IO ()
dialogLoop _ _ _ totalReps currentRep
    | currentRep >= totalReps = TIO.putStrLn "DIALOGUE ENDED" -- Условие завершения диалога
dialogLoop model1 model2 lastPhrase totalReps currentRep = do
    -- Определение текущей модели и оппонента на основе четности номера реплики
    let currentModel = if even currentRep then model1 else model2
    let opponentModel = if even currentRep then model2 else model1
    let speaker = if even currentRep then "Model 1" else "Model 2"
    
    -- Генерация ответа текущей модели на последнюю фразу
    response <- generateResponse currentModel lastPhrase
     -- Вывод реплики с указанием модели
    TIO.putStrLn $ T.pack speaker `T.append` ": " `T.append` T.unwords response
    -- Рекурсивный вызов с обновленными параметрами
    dialogLoop model1 model2 response totalReps (currentRep + 1)

--генерация ответа модели на основе входной фразы
generateResponse :: NGramModel -> [Text] -> IO [Text]
generateResponse model phrase = do
    -- Получаем возможные ключи в порядке: 3 слова -> 2 слова -> 1 слово
    let possibleKeys = getPossibleKeys phrase
    
    -- Пытаемся найти валидный ключ в порядке убывания длины
    case findValidKey model possibleKeys of
        Just key -> do -- Если найден подходящий ключ
            maxLen <- randomRIO (3, 15) -- Случайная длина ответа от 3 до 15 слов
            result <- generateContinuation model key maxLen -- Генерация продолжения
            return result
            
        Nothing -> do -- Если не найден подходящий ключ
            -- Попытка получить случайный ключ из модели
            allKeys <- getRandomKey model
            case allKeys of
                Just randomKey -> do
                    maxLen <- randomRIO (3, 15)  -- Случайная длина для случайного ответа
                    result <- generateContinuation model randomKey maxLen
                    return ("!" : result)  -- Помечаем случайное предложение
                Nothing -> return ["..."]  -- Если совсем нет ключей

-- Безопасное чтение файла с обработкой ошибок
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe fileName = catch (readFile fileName >>= return . Right) handleError
  where
    handleError :: IOException -> IO (Either String String)
    handleError e = do
        let errorMsg = "File reading error: " ++ show e -- Формирование сообщения об ошибке
        return $ Left errorMsg -- Возврат ошибки

-- Безопасное чтение чисел с значением по умолчанию
safeRead :: String -> Int -> Int
safeRead str defaultValue = case readMaybe str of
    Just n -> n -- Успешное чтение числа
    Nothing -> defaultValue -- Использование значения по умолчанию при ошибке