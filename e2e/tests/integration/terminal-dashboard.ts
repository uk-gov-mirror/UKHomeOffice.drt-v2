import moment from 'moment-timezone'
moment.locale("en-gb");
import { todayAtUtcString as todayAtString } from '../support/time-helpers'

describe('Viewing the terminal dashboard page', () => {

  beforeEach(function () {
    cy.deleteData();
  });

  it("should display a box for every queue in the terminal", () => {
    cy
      .addFlight({
        "SchDT": todayAtString(14, 10),
        "ActChoxDT": todayAtString(14, 10),
        "actPax": 51
      })
      .asABorderForceOfficer()
      .navigateHome()
      .visit("/#terminal/T1/dashboard/15/?start=" + todayAtString(14, 15))
      .get(".pax-bar")
      .contains("51 passengers")
      .get(".time-label")
      .contains("14:15 - 14:30")
      .get(".eeadesk")
      .contains("38 pax joining")
      .get(".eeadesk")
      .contains("6 min wait")
      .get(".eeadesk > :nth-child(4) > .fa")
      .should('have.class', 'fa-arrow-up')
      .get(".next-bar")
      .click()
      .get(".time-label")
      .contains("14:30 - 14:45")
      .get(".eeadesk")
      .contains("10 min wait")
      .get(".eeadesk > :nth-child(4) > .fa")
      .should('have.class', 'fa-arrow-up')
      .get(".prev-bar")
      .click()
      .get(".prev-bar")
      .click()
      .get(".time-label")
      .contains("14:00 - 14:15")
      .get(".eeadesk")
      .contains("0 min wait")
      .get(".eeadesk > :nth-child(4) > .fa")
      .should('have.class', 'fa-arrow-right')
  })

  it("should display flights with PCP time in the window", () => {
    cy
      .addFlight({
        "ICAO": "TS0123",
        "IATA": "TS0123",
        "SchDT": todayAtString(10, 30),
        "ActChoxDT": todayAtString(14, 15),
        "actPax": 300
      })
      .addFlight({
        "ICAO": "TS0124",
        "IATA": "TS0124",
        "SchDT": todayAtString(14, 10),
        "ActChoxDT": todayAtString(14, 30),
        "actPax": 51
      })
      .asABorderForceOfficer()
      .navigateHome()
      .visit("/#terminal/T1/dashboard/15/?start=" + todayAtString(14, 15))
      .get("a.terminal-dashboard-side__sidebar_widget").click()
      .get(".dashboard-arrivals-popup tbody tr").contains("TS0123")
      .get(".dashboard-arrivals-popup tbody tr").should('have.length', 1)
      .get(".popover-overlay")
      .click()
      .get(".next-bar")
      .click()
      .get("a.terminal-dashboard-side__sidebar_widget").click()
      .get(".dashboard-arrivals-popup tbody tr").should('have.length', 2)
      .get(".dashboard-arrivals-popup tbody tr").contains("TS0123")
      .get(".arrivals__table__flight-code > div:nth(1)").contains("TS0124")
      .get(".popover-overlay")
      .click()
      .get(".prev-bar")
      .get("select")
      .select('30')
      .url().should('include', 'dashboard/30')
      .get(".time-label")
      .contains("14:30 - 15:00")
      .get('select')
      .should('have.value', '30')
      .get(".pax-bar")
      .contains("311 passengers")
      .get("a.terminal-dashboard-side__sidebar_widget").click()
      .get(".dashboard-arrivals-popup tbody tr").contains("TS0123")
      .get(".dashboard-arrivals-popup tbody tr").contains("TS0124")
      .get(".dashboard-arrivals-popup tbody tr").should('have.length', 2)

  })

});
