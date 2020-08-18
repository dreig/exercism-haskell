module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.List (intersect)
import Data.Time.Calendar (Day, fromGregorian, dayOfWeek, gregorianMonthLength)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show, Enum, Ord)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = fromGregorian year month dayNumber
  where daysOfMonth = weekdaysOfMonth weekday year month
        dayNumber = case schedule of
                      First  -> head daysOfMonth
                      Second -> daysOfMonth !! 1
                      Third  -> daysOfMonth !! 2
                      Fourth -> daysOfMonth !! 3
                      Last   -> last daysOfMonth
                      Teenth -> head $ intersect daysOfMonth [13..19]

firstWeekdayOfMonth :: Weekday -> Integer -> Int -> Int
firstWeekdayOfMonth weekday year month =
  let fstDayOfMonth = fromGregorian year month 1
      fstDayWeekday = (`mod` 7) . (+6) $ fromEnum (dayOfWeek fstDayOfMonth)
      -- we subtract 1 above: (`mod` 7) . (+6), because Data.Time.Calendar.Week.DayOfWeek has values in [1..7]
      offset = (7 + fromEnum weekday - fstDayWeekday) `mod` 7
  in 1 + offset

weekdaysOfMonth :: Weekday -> Integer -> Int -> [Int]
weekdaysOfMonth weekday year month =
  let fstDay = firstWeekdayOfMonth weekday year month
  in [fstDay, (fstDay+7) .. gregorianMonthLength year month]
