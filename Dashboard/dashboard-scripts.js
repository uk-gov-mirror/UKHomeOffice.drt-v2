



///////////////////////////////////////////////////
// New scripts below
//////////////////////////////////////////////////
function populate_queue_details (tb) {

    d3.selectAll("div").remove();

    // Extract the passenger numbers for the current time-bucket
    d3.json("./data/Dashboard-DesksQueues.json", function (error,data) {

      function update_queue_numbers(data,tb) {

        d3.select('body').append('div').attr('class','topbar');
        d3.select('.topbar').append('div').attr('class','main-logo');

        d3.select('body').append('div').attr('class','arrival-tooltip')
        

          var totalPax = data[tb].EEA_Pax + data[tb].eGates_Pax + data[tb].nonEEA_Pax + ' passengers presenting at the PCP in the next 15 min';

          // Update the pax numbers for the current timebucket and Arrivals button
          d3.select('body').append('div').attr('class','pax-bar').text(totalPax); 

          d3.select('body').append('div').attr('class','arrivals_btn')
              .on("mouseenter", arrivals_onMouseEnter)
              .on("mouseleave", arrivals_onMouseLeave)
              .text('View Arrivals');
         
         
         d3.select('body').append('div').attr('class','arrival-tooltip')

          //////////////////////////////////
          // add the eGate queue information
          //////////////////////////////////
          d3.select('body').append('div').attr('class','queue-egates');
          d3.select('.queue-egates').append('div').attr('class','queue-1').text('eGates');
          d3.select('.queue-egates').append('div').attr('class','pax-queue-logo');
          d3.select('.queue-egates').append('div').attr('class','egate-pax-queue-text').text(data[tb].eGates_Pax + ' pax joining');
          d3.select('.queue-egates').append('div').attr('class','pax-wait-logo');
          if (data[tb].eGates_EstWait < 1) {
            d3.select('.queue-egates').append('div').attr('class','egate-pax-wait-text').text('No wait time');
          } else {
            d3.select('.queue-egates').append('div').attr('class','egate-pax-wait-text').text(data[tb].eGates_EstWait + ' min wait time');
          }
          if (data[tb].eGates_EstWait > data[tb-1].eGates_EstWait) {
            d3.select('.queue-egates').append('div').attr('class','pax-increase-logo');
          } else if (data[tb].eGates_EstWait < data[tb-1].eGates_EstWait){
            d3.select('.queue-egates').append('div').attr('class','pax-decrease-logo');
          } else {
            d3.select('.queue-egates').append('div').attr('class','pax-neutral-logo');
          }          
          d3.select('.queue-egates').append('div').attr('class','egate-pax-increase-decrease-text').text('queue time');

          // //////////////////////////////////////////////////////////////////////////////////////////////////
          // Note:  the limits for the miniserial breach and standard breach are hard-coded at 60min and 25min
          //        and will need to be parameterised.
          /////////////////////////////////////////////////////////////////////////////////////////////////////
          if (data[tb].eGates_EstWait > 60 ) {
            d3.select('.queue-egates').attr('style','background-color:#ff9999');
            d3.select('.egate-pax-wait-text').attr('style','background-color:#ff9999');
            d3.select('.egate-pax-queue-text').attr('style','background-color:#ff9999');
            d3.select('.egate-pax-increase-decrease-text').attr('style','background-color:#ff9999');
          } else if (data[tb].eGates_EstWait >= 25 && data[tb].eGates_EstWait < 60) {
            d3.select('.queue-egates').attr('style','background-color:#ffd699');
            d3.select('.egate-pax-wait-text').attr('style','background-color:#ffd699');
            d3.select('.egate-pax-queue-text').attr('style','background-color:#ffd699');
            d3.select('.egate-pax-increase-decrease-text').attr('style','background-color:#ffd699');
          }
          

          //////////////////////////////////////
          // add the eea desk queue information
          //////////////////////////////////////          
          d3.select('body').append('div').attr('class','queue-eea');
          d3.select('.queue-eea').append('div').attr('class','queue-2').text('EEA Desks');
          d3.select('.queue-eea').append('div').attr('class','pax-queue-logo');
          d3.select('.queue-eea').append('div').attr('class','eea-pax-queue-text').text(data[tb].EEA_Pax + ' pax joining');
          d3.select('.queue-eea').append('div').attr('class','pax-wait-logo');
          if (data[tb].EEA_EstWait < 1) {
            d3.select('.queue-eea').append('div').attr('class','eea-pax-wait-text').text('No wait time');
          } else {
            d3.select('.queue-eea').append('div').attr('class','eea-pax-wait-text').text(data[tb].EEA_EstWait + ' min wait time');
          }
         if (data[tb].EEA_EstWait > data[tb-1].EEA_EstWait) {
            d3.select('.queue-eea').append('div').attr('class','pax-increase-logo');
          } else if (data[tb].EEA_EstWait < data[tb-1].EEA_EstWait){
            d3.select('.queue-eea').append('div').attr('class','pax-decrease-logo');
          } else {
            d3.select('.queue-eea').append('div').attr('class','pax-neutral-logo');
          }          
          d3.select('.queue-eea').append('div').attr('class','eea-pax-increase-decrease-text').text('queue time');           
 
          // //////////////////////////////////////////////////////////////////////////////////////////////////
          // Note:  the limits for the miniserial breach and standard breach are hard-coded at 60min and 25min
          //        and will need to be parameterised.
          /////////////////////////////////////////////////////////////////////////////////////////////////////
          if (data[tb].EEA_EstWait > 60 ) {
            d3.select('.queue-eea').attr('style','background-color:#ff9999');
            d3.select('.eea-pax-wait-text').attr('style','background-color:#ff9999');
            d3.select('.eea-pax-queue-text').attr('style','background-color:#ff9999');
            d3.select('.eea-pax-increase-decrease-text').attr('style','background-color:#ff9999');
          } else if (data[tb].EEA_EstWait >= 25 && data[tb].EEA_EstWait < 60) {
            d3.select('.queue-eea').attr('style','background-color:#ffd699');
            d3.select('.eea-pax-wait-text').attr('style','background-color:#ffd699');
            d3.select('.eea-pax-queue-text').attr('style','background-color:#ffd699');
            d3.select('.eea-pax-increase-decrease-text').attr('style','background-color:#ffd699');
          }

          //////////////////////////////////////
          // add the noneea desk queue information
          //////////////////////////////////////  
          d3.select('body').append('div').attr('class','queue-noneea');
          d3.select('.queue-noneea').append('div').attr('class','queue-3').text('Non-EEA Desks');
          d3.select('.queue-noneea').append('div').attr('class','pax-queue-logo');
          d3.select('.queue-noneea').append('div').attr('class','noneea-pax-queue-text').text(data[tb].nonEEA_Pax + ' pax joining');
          d3.select('.queue-noneea').append('div').attr('class','pax-wait-logo');
          if (data[tb].nonEEA_EstWait < 1) {
            d3.select('.queue-noneea').append('div').attr('class','noneea-pax-wait-text').text('No wait time');
          } else {
            d3.select('.queue-noneea').append('div').attr('class','noneea-pax-wait-text').text(data[tb].nonEEA_EstWait + ' min wait time');
          }
          if (data[tb].nonEEA_EstWait > data[tb-1].nonEEA_EstWait) {
            d3.select('.queue-noneea').append('div').attr('class','pax-increase-logo');
          } else if (data[tb].nonEEA_EstWait < data[tb-1].nonEEA_EstWait){
            d3.select('.queue-noneea').append('div').attr('class','pax-decrease-logo');
          } else {
            d3.select('.queue-noneea').append('div').attr('class','pax-neutral-logo');
          }          
          d3.select('.queue-noneea').append('div').attr('class','noneea-pax-increase-decrease-text').text('queue time');  
          
          // //////////////////////////////////////////////////////////////////////////////////////////////////
          // Note:  the limits for the miniserial breach and standard breach are hard-coded at 60min and 25min
          //        and will need to be parameterised.
          /////////////////////////////////////////////////////////////////////////////////////////////////////
          if (data[tb].nonEEA_EstWait > 60 ) {
            d3.select('.queue-noneea').attr('style','background-color:#ff9999');
            d3.select('.noneea-pax-wait-text').attr('style','background-color:#ff9999');
            d3.select('.noneea-pax-queue-text').attr('style','background-color:#ff9999');
            d3.select('.noneea-pax-increase-decrease-text').attr('style','background-color:#ff9999');
          } else if (data[tb].nonEEA_EstWait >= 45 && data[tb].nonEEA_EstWait < 60) {
            d3.select('.queue-noneea').attr('style','background-color:#ffd699');
            d3.select('.noneea-pax-wait-text').attr('style','background-color:#ffd699');
            d3.select('.noneea-pax-queue-text').attr('style','background-color:#ffd699');
            d3.select('.noneea-pax-increase-decrease-text').attr('style','background-color:#ffd699');
          }


          // /////////////////////////////////////////
          // Note:  Add the Previous / Next navigation
          ////////////////////////////////////////////
          var next_tb = parseInt(tb) + 1;
          d3.select('body').append('div').attr('class','tb-bar').text(data[parseInt(tb)].TimeBucket + " - " + data[next_tb].TimeBucket);  
          d3.select('body').append('div').attr('class','prev-bar')
            .on('mouseover', function() {
                d3.select('.prev-bar').attr('style','background-color:#6f777b;cursor:pointer');
            })
            .on('mouseout', function() {
                d3.select('.prev-bar').attr('style','background-color:#eee;cursor:default');
            })
            .on('click', populate_previous)
            .text("<<") 
          ;
    
          d3.select('body').append('div').attr('class','next-bar')
            .on('mouseover', function() {
                d3.select('.next-bar').attr('style','background-color:#6f777b;cursor:pointer');
            })
            .on('mouseout', function() {
                d3.select('.next-bar').attr('style','background-color:#eee;cursor:default');
            })
            .on('click', populate_next)
            .text(">>") 
          ;
           
          return;
      }
    
      //Execute the fucnction
      update_queue_numbers(data,tb);     
  }); 
} 

function populate_previous() {
  var current_tb = document.getElementById("timebucket").value;
  var previous_tb = parseInt(current_tb) - 1;
  document.getElementById("timebucket").value = previous_tb;
  populate_queue_details(previous_tb);
  populate_arrivals(previous_tb);
  //populate_passenger_mix(previous_tb);
}

function populate_next() { 
  var current_tb = document.getElementById("timebucket").value;
  var next_tb = parseInt(current_tb) + 1;
  document.getElementById("timebucket").value = next_tb;
  populate_queue_details(next_tb);
  populate_arrivals(next_tb);
  //populate_passenger_mix(next_tb);
}

function arrivals_onMouseEnter() {
  d3.select('.arrivals_btn').attr('style','background-color:#c1dded;cursor:pointer');
  d3.select('.arrival-tooltip').attr('style','opacity:0.95');
}

function arrivals_onMouseLeave() {
  d3.select('.arrivals_btn').attr('style','background-color:#d5e8fe;cursor:default');
  d3.select('.arrival-tooltip').attr('style','opacity:0');

}

function populate_arrivals(tb) {
    
  d3.json("./data/Dashboard-ArrivalsInput.json", function (error,data) {

  function update_arrivals_details(data,columns) {
    
    // only include the flights in the current timeb bucket
    // first filter by the currenthour
    var currentHour = Math.trunc(parseInt(tb) * 0.25);
    var arrivals_currentHour = data.filter(function(data) {return parseInt(data.EstPCP) == currentHour});

    // second filter on the 15 min time bucket
    var currentMinute = document.getElementsByClassName('tb-bar')[0].innerHTML.substring(3,5);

    switch(currentMinute) {
      case '00':
          var arrivals_in_timebucket = arrivals_currentHour.filter(function(arrivals_currentHour) { return parseInt(arrivals_currentHour.EstPCP.substring(3,5)) <= 15});
        break;
      case '15':
          var arrivals_in_timebucket = arrivals_currentHour.filter(function(arrivals_currentHour) { return parseInt(arrivals_currentHour.EstPCP.substring(3,5)) >15 && parseInt(arrivals_currentHour.EstPCP.substring(3,5)) <= 30});
        break;
      case '30':
          var arrivals_in_timebucket = arrivals_currentHour.filter(function(arrivals_currentHour) { return parseInt(arrivals_currentHour.EstPCP.substring(3,5)) >30 && parseInt(arrivals_currentHour.EstPCP.substring(3,5)) <= 45});
        break;
      case '45':
          var arrivals_in_timebucket = arrivals_currentHour.filter(function(arrivals_currentHour) { return parseInt(arrivals_currentHour.EstPCP.substring(3,5)) > 45});
        break;      
    }

    var table = d3.select('.arrival-tooltip').append('table').attr('id','arrivals')
    var thead = table.append('thead')
    var	tbody = table.append('tbody');
  
    // append the header row
    thead.append('tr')
        .selectAll('th')
        .data(columns).enter()
        .append('th')
        .attr('width', function(column) {
            switch(column) {
              case 'GateStand':
                width = '13%'
                break;
              case 'EstChox':
                  width = '10%'
                  break;
              case 'ActChox':
                  width = '10%'
                  break;
              case 'EstPCP':
                  width = '10%'
                  break;
    
              default:
                width = '7%'
           }
            return width; 
          
        })
        .text(function (column) { 
            switch(column) {
              case 'IATA':
                name = 'Flight'
                break;
              case 'GateStand':
                name = 'Gate / Stand'
                break;
              case 'ScheduledTime':
                  name = 'Scheduled'
                  break;
              case 'EstArrival':
                  name = 'Est'
                  break;
              case 'ActArrival':
                    name = 'Act'
                    break;
              case 'EstChox':
                    name = 'Est Chox'
                    break;
              case 'ActChox':
                  name = 'Act Chox'
                  break;
              case 'EstPCP':
                  name = 'Est PCP'
                  break;
              case 'PCPPax':
                  name = 'Pax'
                  break;
              case 'API_eGates':
                  name = 'eGates'
                  break;
              case 'API_EEA':
                  name = 'EEA'
                  break;
              case 'API_NonEEA':
                  name = 'NonEEA'
                  break;
              default:
                name = column
           }
            return name;  
        });
       
      // create a row for each object in the data
      var rows = tbody.selectAll('tr')
        .data(arrivals_in_timebucket)
        .enter()
        .append('tr');
  
      // create a cell in each row for each column
      var cells = rows.selectAll('td')
        .data(function (row) {
          return columns.map(function (column) {
            return {column: column, value: row[column]};
          });
        })
        .enter()
        .append('td')
          .text(function (d) { return d.value; });
  
      return table;
  }

  // Execute the function
  update_arrivals_details(data, ['IATA','Origin','GateStand','ScheduledTime','EstArrival','ActArrival','EstChox','ActChox','EstPCP','PCPPax','API_eGates','API_EEA','API_NonEEA']); // 2 column table

});
}

function populate_passenger_mix(tb) {
    
  d3.json("./data/Dashboard-PassengerInput.json", function (error,data) {

  function update_passenger_numbers(data) {
    
    // only include passengers from the flights in the current timeb bucket
    // first filter by the currenthour
    var currentHour = Math.trunc(parseInt(tb) * 0.25);
    var pax_currentHour = data.filter(function(data) {return parseInt(data.pcp.substring(11,13)) == currentHour});
    
    // second filter on the 15 min time bucket
    var currentMinute = document.getElementsByClassName('tb-bar')[0].innerHTML.substring(3,5);
    //alert (currentHour + ' ' + currentMinute)
  console.log('Hour and Minute Pax:' + currentHour + ' ' + currentMinute)
    //alert(pax.pax_currentHour[0].pcp)
    switch(currentMinute) {
      case '00':
          var pax_in_timebucket = pax_currentHour.filter(function(pax_currentHour) { return parseInt(pax_currentHour.pcp.substring(14,16)) <= 15});
        break;
      case '15':
          var pax_in_timebucket = pax_currentHour.filter(function(pax_currentHour) { return parseInt(pax_currentHour.pcp.substring(14,16)) >15 && parseInt(pax_currentHour.pcp.substring(14,16)) <= 30});
        break;
      case '30':
          var pax_in_timebucket = pax_currentHour.filter(function(pax_currentHour) { return parseInt(pax_currentHour.pcp.substring(14,16)) >30 && parseInt(pax_currentHour.pcp.substring(14,16)) <= 45});
          break;
      case '45':
         var pax_in_timebucket = pax_currentHour.filter(function(pax_currentHour) { return parseInt(pax_currentHour.pcp.substring(14,16)) > 45});
        break;      
    }

    //////////////////////////////////////////////////////////
    // Count for each of the passenger "types"
    // EEA non-machine readable - EEA with document_type = 'I'
    // EEA Machine readable - EEA with document_type = 'P'
    // Transit - in_transit_flag = 'Y'
    // Visa Nationals - 
    // Non-Visa Nationals - 
    // B5JSSK
    // B5JSSK under 11
    // EEA under 11
    /////////////////////////////////////////////////////////

    var b5jssk_list = ['USA', 'AUS', 'CAN', 'NZL', 'JPN', 'KOR', 'SGP'];
    var eea_list = ['GBR', 'AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE', 'GBR', 'ISL', 'NOR', 'LIE', 'CHE'];
    var vn_list = ['AFG','AGO','ALB','ARM','AZE','BDI','BEN','BES','BFA','BGD','BHR','BIH','BLR','BOL','BTN','CAF','CHN','CIV','CMR','COD','COG','COL','COM','CPV','CUB','CUW','DJI','DOM','DZA','ECU','EGY','ERI','ESH','ETH','FJI','GAB','GEO','GGY','GHA','GIN','GMB','GNB','GNQ','GUY','HTI','IDN','IND','IRN','IRQ','JAM','JOR','KAZ','KEN','KGZ','KHM','LAO','LBN','LBR','LBY','LKA','LSO','MAR','MDA','MDG','MKD','MLI','MMR','MNE','MNG','MOZ','MRT','MWI','NER','NGA','NPL','PAK','PER','PHL','PRK','PSE','RUS','RWA','SAU','SDN','SEN','SLE','SOM','SRB','SSD','STP','SUR','SWZ','SXM','SYR','TCD','TGO','THA','TJK','TKM','TMP','TUN','TUR','TZA','UGA','UKR','UNA','UNO','UZB','VAT','VEN','VNM','XXB','YEM','ZAF','ZMB','ZWE'];
    var nvn_list = ['ABW','AIA','ALA','AND','ANT','ARG','ASM','ATA','ATF','ATG','AUS','BHS','BLM','BLZ','BMU','BRA','BRB','BRN','BVT','BWA','CAN','CCK','CHL','COK','CRI','CXR','CYM','DMA','FLK','FRO','FSM','GIB','GLP','GRD','GRL','GTM','GUF','GUM','HKG','HMD','HND','IMN','IOT','ISL','ISR','JEY','JPN','KIR','KNA','KOR','LCA','LIE','MAC','MAF','MCO','MDV','MEX','MHL','MNP','MSR','MTQ','MUS','MYS','MYT','NAM','NCL','NFK','NIC','NIU','NRU','NZL','PAN','PCN','PLW','PNG','PRI','PRY','PYF','REU','SGP','SGS','SHN','SJM','SLB','SLV','SMR','SPM','SUN','SYC','TCA','TKL','TLS','TON','TTO','TUV','TWN','UMI','URY','USA','VCT','VGB','VIR','VUT','WLF','WSM'];
    
    var transit_count = 0; // A
    var b5jssk_eligilbe_count = 0; // B
    var b5jssk_ineligible_count = 0; // C
    var eea_eligilbe_count = 0; // D
    var eea_ineligible_count = 0; // E
    var eea_machine_count = 0; // F
    var eea_nonmachine_count = 0;  // G
    var nvn_count = 0; // H
    var vn_count = 0;  // I  

    for(var i=0; i<pax_in_timebucket.length; i++){
      //Transit
      if (pax_in_timebucket[i].in_transit_flag == 'Y') {
        transit_count++;
      };
      
      // B5JSSK 
      if (b5jssk_list.includes(pax_in_timebucket[i].nationality_country_code)) {
        if (parseInt(pax_in_timebucket[i].age) < 11) {
          b5jssk_ineligible_count++;
        } else {
          b5jssk_eligilbe_count++;
        }
      }
    
      // EEA 
      if (eea_list.includes(pax_in_timebucket[i].nationality_country_code)) {
          if (parseInt(pax_in_timebucket[i].age) < 11) {
            eea_ineligible_count++;
          } else {
            eea_eligilbe_count++;
          }

          if (parseInt(pax_in_timebucket[i].docuument_type) =='P') {
              eea_machine_count++;
          } else {
              eea_nonmachine_count++;
          }
      }  

      // Visa Nationals
      if (vn_list.includes(pax_in_timebucket[i].nationality_country_code)) {
          vn_count++;
      }
      // Non-visa Nationals
      if (nvn_list.includes(pax_in_timebucket[i].nationality_country_code)) {
        nvn_count++;
      }
    }

    d3.select('body').append('div').attr('id','svg-container');
    d3.select('body').append('div').attr('class','svg-tooltip')

  	// set the dimensions and margins of the graph
    var width = 300  
		 height = 300
			margin = 30
		var radius = Math.min(width, height) / 2 - margin


		// append the svg object to the div called 'my_dataviz'
		var svg = d3.select('#svg-container')
      .append('svg')
      .attr('id','pax-mix')
      .attr('width', width)
      .attr('height', height)
			.append('g')
      .attr('transform', 'translate(' + width / 2 + ',' + height / 2 + ')');

			// create 2 data_set
			var data1 = {a: 9, b: 20, c:30, d:8, e:12}
			var data2 = {a: 6, b: 16, c:20, d:14, e:19, f:12}
      
      var data3 = {a: b5jssk_eligilbe_count, b: eea_eligilbe_count, c: b5jssk_ineligible_count, d: eea_ineligible_count, e: nvn_count, f: vn_count}
      
      
      //alert(transit_count + ' ' + b5jssk_eligilbe_count + ' ')
			// set the color scale
			var color = d3.scaleOrdinal()
			  .domain(['a', 'b', 'c', 'd', 'e', 'f'])
			  .range(d3.schemeDark2);
	
  
      // Compute the position of each group on the pie:
      var pie = d3.pie()
        .value(function(d) {return d.value; })
        .sort(function(a, b) { console.log(a) ; return d3.ascending(a.key, b.key);} ) // This make sure that group order remains the same in the pie chart
        var data_ready = pie(d3.entries(data3))

      // map to data
      var u = svg.selectAll('path')
         .data(data_ready)

      // Build the pie chart: Basically, each part of the pie is a path that we build using the arc function.
      u
      .enter()
      .append('path')
      //.on("mouseenter", function(d){ return(color(d.data.key)) })
      .on("mouseenter", pax_onMouseEnter)
      .on("mouseleave", pax_onMouseLeave)
      .merge(u)
      .transition()
      .attr('d', d3.arc()
       .innerRadius(0)
        .outerRadius(radius)
      )
      .attr('fill', function(d){ return(color(d.data.key)) })
      .attr('stroke', 'white')
      .style('stroke-width', '2px')
      .style('opacity', 1)

    

    return;
  }

  // Execute the function
  update_passenger_numbers(data);

});
}

function pax_onMouseEnter() {
  d3.select('#svg-container').attr('style','cursor:pointer');
  d3.select('.svg-tooltip').attr('style','opacity:0.95');
}

function pax_onMouseLeave() {
  d3.select('#svg-container').attr('style','cursor:default');
  d3.select('.svg-tooltip').attr('style','opacity:0');

}