package services.`export`

import controllers.application.exports.CsvFileStreaming
import drt.shared.PortCode
import drt.shared.Terminals.T1
import org.specs2.mutable.Specification
import services.SDate
import services.graphstages.Crunch

class ExportsSpec extends Specification {
  "Given a start date of midnight BST 2020-06-24 and an end date of 1 minute before midnight BST (24 hours)" >> {
    "When I ask for a filename for the export" >> {
      "I should get a file name with just the start date 2020-06-24" >> {
        val startDate = SDate("2020-06-24T00:00", Crunch.europeLondonTimeZone)
        val endDate = startDate.addDays(1).addMinutes(-1)
        val result = CsvFileStreaming.makeFileName("mysubject", T1, startDate, endDate, PortCode("LHR"))

        val expected = "LHR-T1-mysubject-2020-06-24"

        result === expected
      }
    }
  }

  "Given a start date of midnight UTC 2020-01-01 and an end date of 1 minute before midnight UTC (24 hours)" >> {
    "When I ask for a filename for the export" >> {
      "I should get a file name with just the start date" >> {
        val startDate = SDate("2020-01-01T00:00", Crunch.europeLondonTimeZone)
        val endDate = startDate.addDays(1).addMinutes(-1)
        val result = CsvFileStreaming.makeFileName("mysubject", T1, startDate, endDate, PortCode("LHR"))

        val expected = "LHR-T1-mysubject-2020-01-01"

        result === expected
      }
    }
  }

  "Given a start date of midnight BST 2020-06-24 and an end date of 1 minute before midnight the following day BST (2 days)" >> {
    "When I ask for a filename for the export" >> {
      "I should get a file name with the start date 2020-06-24 and end date of 2020-06-25" >> {
        val startDate = SDate("2020-06-24T00:00", Crunch.europeLondonTimeZone)
        val endDate = startDate.addDays(2).addMinutes(-1)
        val result = CsvFileStreaming.makeFileName("mysubject", T1, startDate, endDate, PortCode("LHR"))

        val expected = "LHR-T1-mysubject-2020-06-24-to-2020-06-25"

        result === expected
      }
    }
  }

  "Given a start date of midnight UTC 2020-01-01 and an end date of 1 minute before midnight the following day UTC (2 days)" >> {
    "When I ask for a filename for the export" >> {
      "I should get a file name with the start date 2020-01-01 and end date of 2020-01-02" >> {
        val startDate = SDate("2020-01-01T00:00", Crunch.europeLondonTimeZone)
        val endDate = startDate.addDays(2).addMinutes(-1)
        val result = CsvFileStreaming.makeFileName("mysubject", T1, startDate, endDate, PortCode("LHR"))

        val expected = "LHR-T1-mysubject-2020-01-01-to-2020-01-02"

        result === expected
      }
    }
  }
}
