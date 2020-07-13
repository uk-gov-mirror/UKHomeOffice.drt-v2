import moment from 'moment-timezone'
moment.locale("en-gb");

import { todayAtUtcString } from '../support/time-helpers'
import { todayAtUtc } from '../support/time-helpers';

describe('Arrivals page', () => {

  beforeEach(function () {
    cy.deleteData();
  });

  const ukPassport = {
    "DocumentIssuingCountryCode": "GBR",
    "PersonType": "P",
    "DocumentLevel": "Primary",
    "Age": "30",
    "DisembarkationPortCode": "TST",
    "InTransitFlag": "N",
    "DisembarkationPortCountryCode": "TST",
    "NationalityCountryEEAFlag": "EEA",
    "PassengerIdentifier": "",
    "DocumentType": "Passport",
    "PoavKey": "1",
    "NationalityCountryCode": "GBR"
  };

  const inTransitPassenger = {
    "DocumentIssuingCountryCode": "GBR",
    "PersonType": "P",
    "DocumentLevel": "Primary",
    "Age": "30",
    "DisembarkationPortCode": "TST",
    "InTransitFlag": "Y",
    "DisembarkationPortCountryCode": "TST",
    "NationalityCountryEEAFlag": "EEA",
    "PassengerIdentifier": "",
    "DocumentType": "Passport",
    "PoavKey": "1",
    "NationalityCountryCode": "GBR"
  };

  const manifest = (passengerList): object => {
    const scheduledDateTime = todayAtUtc(0, 55);
    const schDateString = scheduledDateTime.format("YYYY-MM-DD");
    const schTimeString = scheduledDateTime.format('HH:mm:00');

    return {
      "EventCode": "DC",
      "DeparturePortCode": "AMS",
      "VoyageNumberTrailingLetter": "",
      "ArrivalPortCode": "TST",
      "DeparturePortCountryCode": "MAR",
      "VoyageNumber": "0123",
      "VoyageKey": "key",
      "ScheduledDateOfDeparture": schDateString,
      "ScheduledDateOfArrival": schDateString,
      "CarrierType": "AIR",
      "CarrierCode": "TS",
      "ScheduledTimeOfDeparture": "06:30:00",
      "ScheduledTimeOfArrival": schTimeString,
      "FileId": "fileID",
      "PassengerList": passengerList
    }
  }

  it('Displays a flight after it has been ingested via the live feed', () => {
    cy
      .addFlight(
        {
          "SchDT": todayAtUtcString(0, 55),
          "EstDT": todayAtUtcString(1, 5),
          "EstChoxDT": todayAtUtcString(1, 11),
          "ActDT": todayAtUtcString(1, 7),
          "ActChoxDT": todayAtUtcString(1, 12)
        }
      )
      .asABorderForceOfficer()
      .waitForFlightToAppear("TS0123")
      .get('.before-now > :nth-child(2) > span > span')
      .should('have.attr', 'title', 'Schiphol, Amsterdam, Netherlands')
  });

  const ukPassengerNoDocType = {
    "DocumentIssuingCountryCode": "GBR",
    "PersonType": "P",
    "DocumentLevel": "Primary",
    "Age": "30",
    "DisembarkationPortCode": "TST",
    "InTransitFlag": "N",
    "DisembarkationPortCountryCode": "TST",
    "NationalityCountryEEAFlag": "EEA",
    "PassengerIdentifier": "",
    "DocumentType": "",
    "PoavKey": "1",
    "NationalityCountryCode": "GBR"
  };
  const euPassengerWithPassportInsteadOfPDocType = {
    "DocumentIssuingCountryCode": "FRA",
    "PersonType": "P",
    "DocumentLevel": "Primary",
    "Age": "30",
    "DisembarkationPortCode": "TST",
    "InTransitFlag": "N",
    "DisembarkationPortCountryCode": "TST",
    "NationalityCountryEEAFlag": "EEA",
    "PassengerIdentifier": "",
    "DocumentType": "passport",
    "PoavKey": "1",
    "NationalityCountryCode": "GBR"
  };
  const passengerListBadDocTypes = [
    ukPassengerNoDocType,
    euPassengerWithPassportInsteadOfPDocType
  ];

  it('Handles manifests where the doctype is specified incorectly or left off', () => {
    const eGatesCellSelector = ':nth-child(12) > span';
    const eeaCellSelector = ':nth-child(13) > span';
    const nonEeaCellSelector = ':nth-child(14) > span';

    cy
      .addFlight({
        "SchDT": todayAtUtcString(0, 55),
        "ActChoxDT": todayAtUtcString(0, 55),
        "actPax": 2,
      })
      .asABorderForceOfficer()
      .waitForFlightToAppear("TS0123")
      .addManifest(manifest(passengerListBadDocTypes))
      .get('.pax-api')
      .get(eGatesCellSelector)
      .contains("2")
      .get(eeaCellSelector)
      .contains("0")
      .get(nonEeaCellSelector)
      .contains("0")
  });

  it('Uses passenger numbers calculated from API data if no live pax number exists', () => {

    const totalPaxSelector = ':nth-child(11) > .right';

    cy
      .addFlight({
        "ICAO": "TS0123",
        "IATA": "TS0123",
        "SchDT": todayAtUtcString(0, 55),
        "ActChoxDT": todayAtUtcString(0, 55),
        "actPax": 0,
        "maxPax": 0,
      })
      .asABorderForceOfficer()
      .waitForFlightToAppear("TS0123")
      .get(totalPaxSelector)
      .contains("0")
      .addManifest(manifest(
        [
          ukPassport,
          ukPassport
        ]
      ))
      .get('.pax-api')
      .contains("2")

  });

  const ukPassportWithIdentifier = (id): object => {
    return {
      "DocumentIssuingCountryCode": "GBR",
      "PersonType": "P",
      "DocumentLevel": "Primary",
      "Age": "30",
      "DisembarkationPortCode": "TST",
      "InTransitFlag": "N",
      "DisembarkationPortCountryCode": "TST",
      "NationalityCountryEEAFlag": "EEA",
      "PassengerIdentifier": id,
      "DocumentType": "Passport",
      "PoavKey": "1",
      "NationalityCountryCode": "GBR"
    };
  }

  it('only counts each passenger once if API data contains multiple entries for each passenger', () => {

    const totalPaxSelector = ':nth-child(11) > .right';
    cy
      .addFlight({
        "SchDT": todayAtUtcString(0, 55),
        "actPax": 0,
        "maxPax": 0,
      })
      .asABorderForceOfficer()
      .waitForFlightToAppear("TS0123")
      .get(totalPaxSelector)
      .contains("0")
      .addManifest(manifest(
        [
          ukPassportWithIdentifier("id1"),
          ukPassportWithIdentifier("id1"),
          ukPassportWithIdentifier("id2"),
          ukPassportWithIdentifier("id2")
        ]
      ))
      .get('.pax-api')
      .contains("2")
  });

  it('does not add transit passengers to the total pax when using API pax', () => {

    const totalPaxSelector = ':nth-child(11) > .right';
    cy
      .addFlight({
        "SchDT": todayAtUtcString(0, 55),
        "actPax": 0,
        "maxPax": 0,
      })
      .asABorderForceOfficer()
      .waitForFlightToAppear("TS0123")
      .get(totalPaxSelector)
      .contains("0")
      .addManifest(manifest(
        [
          ukPassport,
          ukPassport,
          inTransitPassenger,
          inTransitPassenger
        ]
      ))
      .get('.pax-api')
      .contains("2")
  });

});
