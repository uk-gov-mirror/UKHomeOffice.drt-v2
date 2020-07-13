import moment from 'moment-timezone'

moment.locale("en-gb");

import {manifestForDateTime, passengerList} from '../support/manifest-helpers'
import {todayAtUtcString} from '../support/time-helpers'
import {todayAtUtc} from '../support/time-helpers';

describe('Arrivals CSV Export', () => {

  const scheduledDateTime = todayAtUtc(0, 55);
  const schDateString = scheduledDateTime.format("YYYY-MM-DD");
  const schTimeString = scheduledDateTime.format('HH:mm:00');
  const estTime = todayAtUtc(1, 5);
  const actTime = todayAtUtc(1, 7);
  const estChoxTime = todayAtUtc(1, 11);
  const actChoxTime = todayAtUtc(1, 12);

  const millis = scheduledDateTime.unix() * 1000;

  beforeEach(function () {
    cy.deleteData();
  });

  const manifest = (passengerList): object => manifestForDateTime(schDateString, schTimeString, passengerList)

  const schDateLocal = scheduledDateTime.tz("Europe/London").format("YYYY-MM-DD");
  const schTimeLocal = scheduledDateTime.tz("Europe/London").format("HH:mm");
  const estTimeLocal = estTime.tz("Europe/London").format("HH:mm");
  const actTimeLocal = actTime.tz("Europe/London").format("HH:mm");
  const estChoxTimeLocal = estChoxTime.tz("Europe/London").format("HH:mm");
  const actChoxTimeLocal = actChoxTime.tz("Europe/London").format("HH:mm");
  const pcpTimeLocal = actChoxTime.add(13, "minutes").tz("Europe/London").format("HH:mm");
  const headersWithoutActApi = "IATA,ICAO,origin,gate/stand,status," +
    "scheduled Date,scheduled Time,Est Arrival,Act Arrival,Est Chox,Act Chox,Est PCP," +
    "Total Pax,PCP Pax," +
    "API e-Gates,API EEA,API Non-EEA,API Fast Track," +
    "Historical e-Gates,Historical EEA,Historical Non-EEA,Historical Fast Track," +
    "terminal Average e-Gates,terminal Average EEA,terminal Average Non-EEA,terminal Average Fast Track";
  const actApiHeaders = "API actual - B5JSSK to Desk,API actual - B5JSSK to eGates" +
  ",API actual - EEA (Machine Readable),API actual - EEA (Non Machine Readable)," +
  "API actual - Fast Track (Non Visa),API actual - Fast Track (Visa),API actual " +
  "- Non EEA (Non Visa),API actual - Non EEA (Visa),API actual - Transfer,API actual - eGates";
  const headersWithActApi = headersWithoutActApi + "," + actApiHeaders;
  const totalPax = "51";
  const eGatePax = "25";
  const eeaDesk = "9";
  const nonEEADesk = "17";
  const dataWithoutActApi = "TS0123,TS0123,AMS,46/44R,On Chox," +
    schDateLocal + "," + schTimeLocal + "," + estTimeLocal + "," + actTimeLocal + "," + estChoxTimeLocal + "," + actChoxTimeLocal + "," + pcpTimeLocal + "," +
    totalPax + "," + totalPax + "," +
    eGatePax + "," + eeaDesk + "," + nonEEADesk + ",," +
    ",,,," +
    "13,37,1,";
  const actApiData = "4.0,6.0,5.0,0.0,0.0,0.0,7.0,10.0,0.0,19.0";

  const dataWithActApi = dataWithoutActApi + "," + actApiData;

  const csvWithNoApiSplits = headersWithoutActApi + "\n" + dataWithoutActApi + "\n";
  const csvWithAPISplits = headersWithActApi + "\n" + dataWithActApi + "\n";

  it('Does not show API splits in the flights export for regular users', () => {
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
      .addManifest(manifest(passengerList(24, 10, 7, 10)))
      .get('.pax-api')
      .get('#export-day-arrivals')
      .then((el) => {
        const href = el.prop('href')
        cy.request({
          method: 'GET',
          url: href,
        }).then((resp) => {
          expect(resp.body).to.equal(csvWithNoApiSplits, "Api splits incorrect for regular users")
        })
      })
  });

  it('Allows you to view API splits in the flights export for users with api:view permission', () => {
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
      .addManifest(manifest(passengerList(24, 10, 7, 10)))
      .get('.pax-api')
      .asABorderForceOfficerWithRoles(["api:view"])
      .get('#export-day-arrivals')
      .then((el) => {
        const href = el.prop('href')
        cy.request({
          method: 'GET',
          url: href,
        }).then((resp) => {
          expect(resp.body).to.equal(csvWithAPISplits, "Api splits incorrect for users with API reporting role")
        })
      })
  });
});

