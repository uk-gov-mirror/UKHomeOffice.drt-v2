package services

import org.specs2.mutable.Specification
import services.graphstages.Crunch

class UtcDateSpec extends Specification {
  "When I ask for a UtcDate in BST" >> {
    "Given a BST date/time of midnight 2020-06-25 created as a UTC SDate" >> {
      val date = SDate("2020-06-24T23:00")
      "I should get (2020, 6, 24)" >> {
        val result = UtcDate(date)
        result === UtcDate(2020, 6, 24)
      }
    }
    "Given a BST date/time of 1 minute before midnight 2020-06-25 created as a UTC SDate" >> {
      val date = SDate(2020, 6, 24, 22, 59)
      "I should get (2020, 6, 24)" >> {
        val result = UtcDate(date)
        result === UtcDate(2020, 6, 24)
      }
    }
    "Given a BST date/time of midnight 2020-06-25 created as a Local SDate" >> {
      val date = SDate("2020-06-25T00:00", Crunch.europeLondonTimeZone)
      "I should get (2020, 6, 24)" >> {
        val result = UtcDate(date)
        result === UtcDate(2020, 6, 24)
      }
    }
    "Given a UTC date/time of midnight 2020-02-25 created as a UTC SDate" >> {
      val date = SDate("2020-02-25T00:00")
      "I should get (2020, 2, 25)" >> {
        val result = UtcDate(date)
        result === UtcDate(2020, 2, 25)
      }
    }
    "Given a UTC date/time of midnight 2020-02-25 created as a Local SDate" >> {
      val date = SDate("2020-02-25T00:00", Crunch.europeLondonTimeZone)
      "I should get (2020, 2, 25)" >> {
        val result = UtcDate(date)
        result === UtcDate(2020, 2, 25)
      }
    }
  }
}
