module Meetup (Weekday(..), Schedule(..), meetupDay, firstWeekdayOfMonth,weekdaysOfMonth) where

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

{-
  Returns the date of the month of the first Weekday supplied. (result is in [1..7])
  ex:
  firstWeekdayOfMonth Monday 2020 8     => 3
  firstWeekdayOfMonth Friday 2020 8     => 7
  firstWeekdayOfMonth Saturday 2020 8   => 1
-}
firstWeekdayOfMonth :: Weekday -> Integer -> Int -> Int
firstWeekdayOfMonth weekday year month =
  let fstDayOfMonth = fromGregorian year month 1
      fstDayWeekday = fromEnum (dayOfWeek fstDayOfMonth) -- this is in range [1..7]
      offset = (7 + (fromEnum weekday + 1) - fstDayWeekday) `mod` 7
      -- we add +1 above because Data.Time.Calendar.Week.DayOfWeek has values in [1..7]
  in 1 + offset

{-
  Returns all dates of the month that are the supplied Weekday. (result is in [1..31])
  ex:
  weekdaysOfMonth Monday 2020 8     => [3,10,17,24,31]
  weekdaysOfMonth Friday 2020 8     => [7,14,21,28]
  weekdaysOfMonth Saturday 2020 8   => [1,8,15,22,29]

-}
weekdaysOfMonth :: Weekday -> Integer -> Int -> [Int]
weekdaysOfMonth weekday year month =
  let fstDay = firstWeekdayOfMonth weekday year month
  in [fstDay, (fstDay+7) .. gregorianMonthLength year month]
