package services.crunch

import drt.shared.CrunchApi.{CrunchMinute, StaffMinute}
import drt.shared.Terminals.T1
import drt.shared._
import services.SDate

import java.util.UUID
import scala.collection.immutable.{List, SortedMap}
import scala.concurrent.duration._

class StaffMinutesSpec extends CrunchTestLike {
  sequential
  isolated

  "Given two consecutive shifts " +
    "When I ask for the PortState " +
    "Then I should see the staff available for the duration of the shifts" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")

    val startDate1 = MilliDate(SDate("2017-01-01T00:00").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:14").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 1, None)
    val startDate2 = MilliDate(SDate("2017-01-01T00:15").millisSinceEpoch)
    val endDate2 = MilliDate(SDate("2017-01-01T00:29").millisSinceEpoch)
    val assignment2 = StaffAssignment("shift b", T1, startDate2, endDate2, 2, None)
    val initialShifts = ShiftAssignments(Seq(assignment1, assignment2))
    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => shiftStart
    ))

    offerAndWait(crunch.shiftsInput, initialShifts)

    val expectedStaff = List.fill(15)(1) ::: List.fill(15)(2)
    val expectedMillis = (shiftStart.millisSinceEpoch to (shiftStart.millisSinceEpoch + 29 * oneMinuteMillis) by oneMinuteMillis).toList

    crunch.portStateTestProbe.fishForMessage(5 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute)
        val staff = minutesInOrder.map(_.shifts).take(expectedStaff.length)
        val staffMillis = minutesInOrder.map(_.minute).take(expectedMillis.length)

        (staffMillis, staff) == Tuple2(expectedMillis, expectedStaff)
    }

    success
  }

  "Given shifts of 0 and 1 staff and a -1 staff movement at the start of the shift" +
    "When I ask for the PortState " +
    "Then I should see zero staff available rather than a negative number" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")
    val startDate1 = MilliDate(SDate("2017-01-01T00:00").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:04").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 0, None)
    val startDate2 = MilliDate(SDate("2017-01-01T00:05").millisSinceEpoch)
    val endDate2 = MilliDate(SDate("2017-01-01T00:09").millisSinceEpoch)
    val assignment2 = StaffAssignment("shift b", T1, startDate2, endDate2, 2, None)
    val initialShifts = ShiftAssignments(Seq(assignment1, assignment2))
    val uuid = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement(T1, "lunch start", MilliDate(shiftStart.millisSinceEpoch), -1, uuid, createdBy = None),
      StaffMovement(T1, "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 1, uuid, createdBy = None)
    )

    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => shiftStart
    ))

    offerAndWait(crunch.shiftsInput, initialShifts)
    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val expectedStaffAvailable = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> 0,
      shiftStart.addMinutes(1).millisSinceEpoch -> 0,
      shiftStart.addMinutes(2).millisSinceEpoch -> 0,
      shiftStart.addMinutes(3).millisSinceEpoch -> 0,
      shiftStart.addMinutes(4).millisSinceEpoch -> 0,
      shiftStart.addMinutes(5).millisSinceEpoch -> 1,
      shiftStart.addMinutes(6).millisSinceEpoch -> 1,
      shiftStart.addMinutes(7).millisSinceEpoch -> 1,
      shiftStart.addMinutes(8).millisSinceEpoch -> 1,
      shiftStart.addMinutes(9).millisSinceEpoch -> 1
    )

    crunch.portStateTestProbe.fishForMessage(2 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute).take(10)
        val staffAvailable = minutesInOrder.map(sm => (sm.minute, sm.available))

        staffAvailable == expectedStaffAvailable
    }

    success
  }

  "Given a shift of 0 staff and a -1 staff movement at the start of the shift " +
    "When I monitor the PortState " +
    "Then I should see 0 staff available, and a movement value of -1" >> {
    val startDate = SDate("2017-01-01T00:00")
    val endDate = SDate("2017-01-01T00:05")

    val uuid = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement(T1, "lunch start", MilliDate(startDate.millisSinceEpoch), -1, uuid, createdBy = None),
      StaffMovement(T1, "lunch end", MilliDate(endDate.millisSinceEpoch), 1, uuid, createdBy = None)
    )

    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => startDate
    ))

    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val minutesToCheck = 5
    val expectedStaffAvailableAndMovements = List.fill(minutesToCheck)((0, -1))

    crunch.portStateTestProbe.fishForMessage(5 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.filter(m => startDate.millisSinceEpoch <= m.minute).sortBy(_.minute).take(minutesToCheck)
        val actualAvailableAndMovements = minutesInOrder.map(m => (m.available, m.movements))

        actualAvailableAndMovements == expectedStaffAvailableAndMovements
    }

    success
  }

  "Given a shift of 10 staff and a -1 staff movement at the start of the shift " +
    "When I monitor the PortState " +
    "Then I should see 9 staff available, and a movement value of -1" >> {
    val startDate = SDate("2017-01-01T00:00")
    val endDate = SDate("2017-01-01T00:04")

    val shift = StaffAssignment("shift a", T1, MilliDate(startDate.millisSinceEpoch), MilliDate(endDate.millisSinceEpoch), 10, None)

    val initialShifts = ShiftAssignments(Seq(shift))

    val uuid = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement(T1, "lunch start", MilliDate(startDate.millisSinceEpoch), -1, uuid, createdBy = None),
      StaffMovement(T1, "lunch end", MilliDate(endDate.addMinutes(1).millisSinceEpoch), 1, uuid, createdBy = None)
    )

    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => startDate
    ))

    offerAndWait(crunch.shiftsInput, initialShifts)
    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val minutesToCheck = 5
    val expectedStaffAvailableAndMovements = List.fill(minutesToCheck)((9, -1))

    crunch.portStateTestProbe.fishForMessage(2 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.filter(m => startDate.millisSinceEpoch <= m.minute).sortBy(_.minute).take(minutesToCheck)
        val actualAvailableAndMovements = minutesInOrder.map(m => (m.available, m.movements))

        actualAvailableAndMovements == expectedStaffAvailableAndMovements
    }

    success
  }

  "Given two staff movement covering the same time period " +
    "When I ask for the PortState " +
    "Then I should see the sum of the movements for the minute they cover" >> {
    val shiftStart = SDate("2017-01-01T00:00Z")

    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val initialMovements = Seq(
      StaffMovement(T1, "lunch start", MilliDate(shiftStart.millisSinceEpoch), -1, uuid1, createdBy = None),
      StaffMovement(T1, "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 1, uuid1, createdBy = None),
      StaffMovement(T1, "lunch start", MilliDate(shiftStart.millisSinceEpoch), -5, uuid2, createdBy = None),
      StaffMovement(T1, "lunch end", MilliDate(shiftStart.addMinutes(15).millisSinceEpoch), 5, uuid2, createdBy = None)
    )

    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => shiftStart
    ))

    offerAndWait(crunch.liveStaffMovementsInput, initialMovements)

    val expectedStaffMovements = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> -6,
      shiftStart.addMinutes(1).millisSinceEpoch -> -6,
      shiftStart.addMinutes(2).millisSinceEpoch -> -6,
      shiftStart.addMinutes(3).millisSinceEpoch -> -6,
      shiftStart.addMinutes(4).millisSinceEpoch -> -6,
      shiftStart.addMinutes(5).millisSinceEpoch -> -6,
      shiftStart.addMinutes(6).millisSinceEpoch -> -6,
      shiftStart.addMinutes(7).millisSinceEpoch -> -6,
      shiftStart.addMinutes(8).millisSinceEpoch -> -6,
      shiftStart.addMinutes(9).millisSinceEpoch -> -6
    )

    crunch.portStateTestProbe.fishForMessage(2 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute)
        val staffMovements = minutesInOrder.filter(_.minute >= shiftStart.millisSinceEpoch).map(sm => (sm.minute, sm.movements)).take(10)

        staffMovements == expectedStaffMovements
    }

    success
  }

  "Given one initial fixed point " +
    "When I remove the fixed point " +
    "Then I should not see zero fixed points in the staff minutes" >> {
    val scheduled = "2017-01-01T00:00Z"
    val shiftStart = SDate(scheduled)

    val startDate1 = MilliDate(SDate("2017-01-01T00:00").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:14").millisSinceEpoch)
    val assignment1 = StaffAssignment("egate monitor", T1, startDate1, endDate1, 2, None)
    val initialFixedPoints = FixedPointAssignments(Seq(assignment1))

    val crunch = runCrunchGraph(TestConfig(
      airportConfig = defaultAirportConfig.copy(
        queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)
      ),
      now = () => shiftStart
    ))

    offerAndWait(crunch.fixedPointsInput, initialFixedPoints)

    offerAndWait(crunch.fixedPointsInput, FixedPointAssignments.empty)

    val expectedFixedPoints = Seq(
      shiftStart.addMinutes(0).millisSinceEpoch -> 0,
      shiftStart.addMinutes(1).millisSinceEpoch -> 0,
      shiftStart.addMinutes(2).millisSinceEpoch -> 0,
      shiftStart.addMinutes(3).millisSinceEpoch -> 0,
      shiftStart.addMinutes(4).millisSinceEpoch -> 0,
      shiftStart.addMinutes(5).millisSinceEpoch -> 0,
      shiftStart.addMinutes(6).millisSinceEpoch -> 0,
      shiftStart.addMinutes(7).millisSinceEpoch -> 0,
      shiftStart.addMinutes(8).millisSinceEpoch -> 0,
      shiftStart.addMinutes(9).millisSinceEpoch -> 0
    )

    crunch.portStateTestProbe.fishForMessage(10 seconds) {
      case ps: PortState =>
        val minutesInOrder = ps.staffMinutes.values.toList.sortBy(_.minute).take(10)
        val fixedPoints = minutesInOrder.map(sm => (sm.minute, sm.fixedPoints))

        fixedPoints == expectedFixedPoints
    }

    success
  }

  "Given some fixed points and no existing port state " +
    "When starting the crunch graph " +
    "Then the forecast crunch probe should receive staff minutes for the max crunch days " >> {

    val scheduled = "2017-01-01T00:00Z"
    val now = SDate(scheduled)
    val startDate1 = MilliDate(SDate("2017-01-01T00:00").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:14").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 50, None)
    val fixedPoints = FixedPointAssignments(Seq(assignment1))

    val crunch = runCrunchGraph(TestConfig(
      now = () => now,
      maxDaysToCrunch = 5
    ))

    offerAndWait(crunch.fixedPointsInput, fixedPoints)

    val expectedStaffMinutes = (0 until 5).map { day =>
      val date = SDate(scheduled).addDays(day).toISODateOnly
      val minutes = (0 until 15).map { minute => SDate(scheduled).addDays(day).addMinutes(minute).millisSinceEpoch }.sorted
      (date, minutes)
    }.toMap

    crunch.portStateTestProbe.fishForMessage(5 seconds) {
      case PortState(_, _, staffMinutes) =>
        val actualMinutes = staffMinutes.values.toSeq.filter(_.fixedPoints == 50).groupBy(m => SDate(m.minute).toISODateOnly).mapValues { minutes =>
          minutes.map(m => SDate(m.minute).millisSinceEpoch).sorted
        }
        actualMinutes == expectedStaffMinutes
    }

    success
  }

  "Given some fixed points and existing port state with out of date fixed points " +
    "When starting the crunch graph " +
    "Then the forecast crunch probe should receive staff minutes for the max crunch days reflecting the actual fixed points" >> {

    val scheduled = "2017-01-01T00:00Z"
    val now = SDate(scheduled)
    val startDate1 = MilliDate(SDate("2017-01-01T00:00").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:14").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 50, None)
    val fixedPoints = FixedPointAssignments(Seq(assignment1))

    val daysToCrunch = 5

    def staffMinutes(days: Int,
                     minutes: Int,
                     startDate: String): SortedMap[TM, StaffMinute] = SortedMap[TM, StaffMinute]() ++ (0 until days).flatMap { day =>
      (0 until minutes).map { minute =>
        val currentMinute = SDate(startDate).addDays(day).addMinutes(minute).millisSinceEpoch
        (TM(T1, currentMinute), StaffMinute(T1, currentMinute, 0, 1, 0))
      }
    }

    val crunch = runCrunchGraph(TestConfig(
      now = () => now,
      maxDaysToCrunch = daysToCrunch,
      initialPortState = Option(PortState(SortedMap[UniqueArrival, ApiFlightWithSplits](), SortedMap[TQM, CrunchMinute](), staffMinutes(daysToCrunch, 15, scheduled)))
    ))

    offerAndWait(crunch.fixedPointsInput, fixedPoints)

    val expectedStaffMinutes = (0 until 5).map { day =>
      val date = SDate(scheduled).addDays(day).toISODateOnly
      val minutes = (0 until 15).map { minute => SDate(scheduled).addDays(day).addMinutes(minute).toISOString() }.sorted
      (date, minutes)
    }.toMap

    crunch.portStateTestProbe.fishForMessage(5 seconds) {
      case PortState(_, _, staffMinutes) =>
        val actualMinutes = staffMinutes.values.toSeq.filter(_.fixedPoints == 50).groupBy(m => SDate(m.minute).toISODateOnly).mapValues { minutes =>
          minutes.map(m => SDate(m.minute).toISOString()).sorted
        }
        actualMinutes == expectedStaffMinutes
    }

    success
  }

  "Given a shift not starting on a minute boundary " +
    "When I inspect the staff minutes " +
    "Then I should see the affected staff minutes rounded to the nearest minute" >> {

    val nowString = "2017-01-01T00:00:15Z"
    val now = SDate(nowString)
    val startDate1 = MilliDate(SDate("2017-01-01T00:00:15").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:02:15").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 50, None)
    val shifts = ShiftAssignments(Seq(assignment1))

    val daysToCrunch = 1

    val crunch = runCrunchGraph(TestConfig(
      now = () => now,
      airportConfig = defaultAirportConfig.copy(queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)),
      maxDaysToCrunch = daysToCrunch,
      initialPortState = Option(PortState(SortedMap[UniqueArrival, ApiFlightWithSplits](), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]()))
    ))

    offerAndWait(crunch.shiftsInput, shifts)

    val expectedIsoMinutes = Seq(
      "2017-01-01T00:00:00Z",
      "2017-01-01T00:01:00Z",
      "2017-01-01T00:02:00Z"
    )

    crunch.portStateTestProbe.fishForMessage(2 seconds, s"Didn't find expected minutes ($expectedIsoMinutes)") {
      case PortState(_, _, staffMinutes) =>
        val isoMinutes = staffMinutes.values.toSeq.sortBy(_.minute).map(m => SDate(m.minute).toISOString())
        isoMinutes == expectedIsoMinutes
    }

    success
  }

  "Given a fixed point not starting on a minute boundary " +
    "When I inspect the staff minutes " +
    "Then I should see the affected staff minutes rounded to the nearest minute" >> {

    val nowString = "2017-01-01T00:00:15Z"
    val now = SDate(nowString)
    val startDate1 = MilliDate(SDate("2017-01-01T00:00:15").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:02:15").millisSinceEpoch)
    val assignment1 = StaffAssignment("shift a", T1, startDate1, endDate1, 50, None)
    val fixedPoints = FixedPointAssignments(Seq(assignment1))

    val daysToCrunch = 1

    val crunch = runCrunchGraph(TestConfig(
      now = () => now,
      airportConfig = defaultAirportConfig.copy(queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)),
      maxDaysToCrunch = daysToCrunch,
      initialPortState = Option(PortState(SortedMap[UniqueArrival, ApiFlightWithSplits](), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]()))
    ))

    offerAndWait(crunch.fixedPointsInput, fixedPoints)

    val expectedIsoMinutes = Seq(
      "2017-01-01T00:00:00Z",
      "2017-01-01T00:01:00Z",
      "2017-01-01T00:02:00Z"
    )

    crunch.portStateTestProbe.fishForMessage(2 seconds, s"Didn't find expected minutes ($expectedIsoMinutes)") {
      case PortState(_, _, staffMinutes) =>
        val isoMinutes = staffMinutes.values.toSeq.sortBy(_.minute).map(m => SDate(m.minute).toISOString())
        isoMinutes == expectedIsoMinutes
    }

    success
  }

  "Given a fixed point not starting on a minute boundary " +
    "When I inspect the staff minutes " +
    "Then I should see the affected staff minutes rounded to the nearest minute" >> {

    val nowString = "2017-01-01T00:00:15Z"
    val now = SDate(nowString)
    val startDate1 = MilliDate(SDate("2017-01-01T00:00:15").millisSinceEpoch)
    val endDate1 = MilliDate(SDate("2017-01-01T00:02:15").millisSinceEpoch)
    val uuid = UUID.randomUUID()
    val staffMovement1 = StaffMovement(T1, "some reason", startDate1, 1, uuid, None, None)
    val staffMovement2 = StaffMovement(T1, "some reason", endDate1, -1, uuid, None, None)

    val daysToCrunch = 1

    val crunch = runCrunchGraph(TestConfig(
      now = () => now,
      airportConfig = defaultAirportConfig.copy(queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1)),
      maxDaysToCrunch = daysToCrunch,
      initialPortState = Option(PortState(SortedMap[UniqueArrival, ApiFlightWithSplits](), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]()))
    ))

    offerAndWait(crunch.liveStaffMovementsInput, Seq(staffMovement1, staffMovement2))

    val expectedIsoMinutes = Seq(
      "2017-01-01T00:00:00Z",
      "2017-01-01T00:01:00Z"
    )

    crunch.portStateTestProbe.fishForMessage(2 seconds, s"Didn't find expected minutes ($expectedIsoMinutes)") {
      case PortState(_, _, staffMinutes) =>
        val isoMinutes = staffMinutes.values.toSeq.sortBy(_.minute).map(m => SDate(m.minute).toISOString())
        isoMinutes == expectedIsoMinutes
    }

    success
  }
}
