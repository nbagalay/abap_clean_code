*&---------------------------------------------------------------------*
*& Report ZNNB_CLEAN_ABAP_BAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT znnb_clean_abap_bad.



"--OBECTIVE--
" Build a program that does the following operations:
"1) For a flight, get customer count of people who's destination is the same as their home country
"//////) Of the results from #1, get count of men between 13 - 55 and women between 18 - 40
"2) Of the results from #1, get the count of business vs private customer
"3) For a flight, get the lowest and highest booking cost
"4) Get a count of passengers different than the starting location of a flight
"5) For a flight, get agent count that has the same zip as flight destination
"6) If Agent count for #5 > 0 and #4 > 5 and #3 highest cost is more than $1,000 then call this a 'Gold Star flight'

TYPES: BEGIN OF ty_output,
         carrid                    TYPE s_carr_id,
         connid                    TYPE s_conn_id,
         fldate                    TYPE s_date,
         passenger_destin_and_home TYPE i,
         passenger_in_business     TYPE i,
         passenger_in_private      TYPE i,
         lowest_airfare            TYPE p DECIMALS 2,
         highest_airfare           TYPE p DECIMALS 2,
         agent_flight_dest         TYPE i,
         is_gold_star_flight       TYPE abap_bool,
       END OF ty_output,

       tt_my_flight_output TYPE STANDARD TABLE OF ty_output WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid fldate.

DATA: business_count               TYPE i,
      lv_p_count                   TYPE i,   "Private Count
      lv_lowest_cost               TYPE s_l_cur_pr,
      lv_highest_cost              TYPE s_l_cur_pr,
      agent_flight_dest            TYPE i,
      lv_passenger_destin_and_home TYPE i.

DATA: w_output     TYPE ty_output,
      w_my_plane_t TYPE tt_my_flight_output.

START-OF-SELECTION.

  SELECT *
    FROM sflight
    INTO TABLE @DATA(lt_sflight).


  LOOP AT lt_sflight INTO DATA(ls_flight).

    "We need for this record the specific booking details found for this flight
    SELECT *
      FROM sbook
      WHERE carrid = @ls_flight-carrid AND
            connid = @ls_flight-connid AND
            fldate = @ls_flight-fldate
      INTO TABLE @DATA(lt_sbook).

    SELECT SINGLE *
      FROM spfli
      WHERE carrid = @ls_flight-carrid AND
            connid = @ls_flight-connid
      INTO @DATA(ls_spfli).

    LOOP AT lt_sbook ASSIGNING FIELD-SYMBOL(<fs_sbook>).

      "SATISFIES GOAL #2
      SELECT *
        FROM scustom
        WHERE id  = @<fs_sbook>-customid
        INTO TABLE @DATA(lt_book_customer).

**      LOOP AT lt_book_customer ASSIGNING FIELD-SYMBOL(<ls_customer_type>).
**        IF <ls_customer_type>-custtype = 'B'.
**          ADD 1 TO lv_business_count.
**        ELSEIF <ls_customer_type>-custtype = 'P'.
**          ADD 1 TO lv_private_count.
**        ENDIF.
**      ENDLOOP.
      LOOP AT lt_book_customer ASSIGNING FIELD-SYMBOL(<ls_customer_type>).
        IF <ls_customer_type>-custtype = 'B'.
          ADD 1 TO business_count.
        ELSEIF <ls_customer_type>-custtype = 'P'.
          ADD 1 TO lv_p_count.
        ENDIF.

        "--#4 Get a count of passengers different than the starting location of a flight
        SELECT SINGLE *
          FROM spfli
          WHERE carrid = @<fs_sbook>-carrid AND
                connid = @<fs_sbook>-connid
          INTO @DATA(ls_spfli_for4).    "Flight Schedule
        IF ls_spfli_for4 IS NOT INITIAL.
          IF ls_spfli-countryfr <> <ls_customer_type>-country.
            ADD 1 TO lv_passenger_destin_and_home.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "--#3 get lowest/highest count
      IF lv_lowest_cost > <fs_sbook>-loccuram.
        lv_lowest_cost = <fs_sbook>-loccuram.
      ELSEIF lv_highest_cost < <fs_sbook>-loccuram.
        lv_highest_cost = <fs_sbook>-loccuram.
      ENDIF.

      "-- AGENCY INFORMATION
      SELECT SINGLE *
        FROM stravelag
        WHERE agencynum = @<fs_sbook>-agencynum
        INTO @DATA(ls_travel_agent).

      "-- #5 Requirement
      IF ls_travel_agent-country = ls_spfli-countryto.
        ADD 1 TO agent_flight_dest.
      ENDIF.

    ENDLOOP.

    "--FILL IN FINAL STRUCTURE
    w_output-carrid                    = ls_flight-carrid.
    w_output-connid                    = ls_flight-connid.
    w_output-fldate                    = ls_flight-fldate.
    w_output-passenger_in_business     = business_count.
    w_output-passenger_in_private      = lv_p_count.
    w_output-passenger_destin_and_home = lv_passenger_destin_and_home.
    w_output-agent_flight_dest         = agent_flight_dest.

    "-- #6: Gold Star Flight Check
    IF agent_flight_dest > 0.
      IF lv_passenger_destin_and_home > 5.
        IF lv_highest_cost > '1000.00'.
          w_output-is_gold_star_flight = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND w_output TO w_my_plane_t.

  ENDLOOP.

  "--DISPLAY AND OUTPUT
