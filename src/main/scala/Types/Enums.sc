object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
def isWorkingDay(d: WeekDay.WeekDay) = ! (d == WeekDay.Sat || d == WeekDay.Sun)
WeekDay.values filter isWorkingDay foreach println

