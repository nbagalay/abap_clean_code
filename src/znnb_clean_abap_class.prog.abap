************************************************************************
*                       Local Class Documentation                      *
*                                                                      *
* Title              : Clean ABAP Class                                *
* Original Creator   : Nick Bagalay                                    *
* Creation Month/Year: April 2023                                      *
* Purpose/Detail     :                                                 *
*                                                                      *
************************************************************************
*                         History of Revisions                         *
************************************************************************
*  Mod  |   Date   |   SAP   | Transport | Specify changes made        *
*       |          | USER ID |           |                             *
*-------|----------|---------|-----------|-----------------------------*
************************************************************************

************************************************************************
*******                   Types Specifications                   *******
************************************************************************

TYPES: BEGIN OF gty_rpt_flight_data_root,
         carrier_id                TYPE s_carr_id,
         connection_id             TYPE s_conn_id,
         flight_date               TYPE s_date,
         passenger_destin_and_home TYPE znnb_flight_pass_dst_and_home,
         passenger_in_business     TYPE znnb_flight_pass_in_buss,
         passenger_in_private      TYPE znnb_flight_pass_in_priv,
         lowest_airfare            TYPE znnb_flight_lowest_airfare,
         highest_airfare           TYPE znnb_flight_highest_airfare,
         agent_flight_dest         TYPE znnb_flight_agent_dest,
         is_gold_star_flight       TYPE znnb_flight_is_goldstar,
       END OF gty_rpt_flight_data_root,

       gtt_rpt_flight_data_root TYPE STANDARD TABLE OF gty_rpt_flight_data_root WITH NON-UNIQUE KEY primary_key
       COMPONENTS carrier_id connection_id flight_date.

************************************************************************
*******               Global Internal Tables Area                *******
************************************************************************

************************************************************************
*******                          Objects                         *******
************************************************************************

************************************************************************
*******               Global Working Storage Area                *******
************************************************************************

************************************************************************
*******                  Local Class Definition                  *******
************************************************************************
CLASS lcl_flight_booking DEFINITION.

  PUBLIC SECTION.

    TYPES: gttr_carrier_id    TYPE RANGE OF s_carr_id,
           gttr_connection_id TYPE RANGE OF s_conn_id,
           gttr_flight_date   TYPE RANGE OF s_fl_date.

    CONSTANTS: BEGIN OF lc_customer_types,
                 business_b TYPE s_custtype VALUE 'B',
                 private_p  TYPE s_custtype VALUE 'P',
               END OF lc_customer_types.

    CONSTANTS: BEGIN OF lc_range_tbl,
                 BEGIN OF sign,
                   include TYPE tvarv_sign VALUE 'I',
                   exclude TYPE tvarv_sign VALUE 'E',
                 END OF sign,

                 BEGIN OF option,
                   equal_to TYPE tvarv_opti VALUE 'EQ',
                 END OF option,
               END OF lc_range_tbl,

               BEGIN OF lc_message_types,
                 error_e TYPE bapi_mtype VALUE 'E',
               END OF lc_message_types.
    METHODS constructor.

    METHODS get_report_flight_data
      EXPORTING
        et_msg_return  TYPE bapiret2_t
        et_flight_data TYPE gtt_rpt_flight_data_root.

  PRIVATE SECTION.

    TYPES: BEGIN OF lty_customer_detail_scustom,
             customer_id   TYPE s_customer,

             customer_type TYPE s_custtype,
             country       TYPE s_country,
           END OF lty_customer_detail_scustom,

           ltt_customer_detail_scustom TYPE STANDARD TABLE OF lty_customer_detail_scustom WITH NON-UNIQUE KEY primary_key
           COMPONENTS customer_id.

    TYPES: BEGIN OF lty_travel_agency_stravelag,
             agency_num TYPE s_agncynum,

             country    TYPE s_country,
           END OF lty_travel_agency_stravelag,

           ltt_travel_agency_stravelag TYPE STANDARD TABLE OF lty_travel_agency_stravelag WITH NON-UNIQUE KEY primary_key
           COMPONENTS agency_num.

    TYPES: BEGIN OF lty_booking_detail_sbook,
             carrier_id         TYPE s_carr_id,
             connection_id      TYPE s_conn_id,
             flight_date        TYPE s_date,
             booking_id         TYPE s_book_id,
             customer_id        TYPE s_customer,
             agency_num         TYPE s_agncynum,
             local_cost         TYPE s_l_cur_pr,
             local_cost_uom     TYPE s_currcode,
             customer_detail_st TYPE lty_customer_detail_scustom,
             travel_agency_st   TYPE lty_travel_agency_stravelag,
           END OF lty_booking_detail_sbook,

           ltt_booking_detail_sbook TYPE STANDARD TABLE OF lty_booking_detail_sbook WITH NON-UNIQUE KEY primary_key
           COMPONENTS carrier_id connection_id flight_date booking_id.

    TYPES: BEGIN OF lty_flight_schedule_spfli,
             carrier_id    TYPE s_carr_id,
             connection_id TYPE s_conn_id,

             country_from  TYPE land1,
             country_to    TYPE land1,
           END OF lty_flight_schedule_spfli,

           ltt_flight_schedule_spfli TYPE STANDARD TABLE OF lty_flight_schedule_spfli WITH NON-UNIQUE KEY primary_key
           COMPONENTS carrier_id connection_id.

    TYPES: BEGIN OF lty_flight_model_data_sflight,
             carrier_id         TYPE s_carr_id,
             connection_id      TYPE s_conn_id,
             flight_date        TYPE s_date,
             booking_detail_tt  TYPE ltt_booking_detail_sbook,
             flight_schedule_st TYPE lty_flight_schedule_spfli,
           END OF lty_flight_model_data_sflight,

           ltt_flight_model_data_sflight TYPE STANDARD TABLE OF lty_flight_model_data_sflight WITH NON-UNIQUE KEY primary_key
           COMPONENTS carrier_id connection_id flight_date.

    CONSTANTS: lc_highest_airfare TYPE s_l_cur_pr VALUE '1000.00'.


    METHODS get_flight_data_from_ecc
      EXPORTING et_flight_data TYPE ltt_flight_model_data_sflight.

ENDCLASS.
************************************************************************
*******                Local Class Implementation                *******
************************************************************************

CLASS lcl_flight_booking IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD get_report_flight_data.
    DATA: ls_rpt_flight_data TYPE gty_rpt_flight_data_root.

    "-- Obtain the Flight ModelData needed
    me->get_flight_data_from_ecc( IMPORTING et_flight_data = DATA(lt_flight_data) ).

    "--CALCULATIONS AND FINAL REPORT
    LOOP AT lt_flight_data ASSIGNING FIELD-SYMBOL(<ls_flight_data>).

      ls_rpt_flight_data = CORRESPONDING #( <ls_flight_data> ).

      LOOP AT <ls_flight_data>-booking_detail_tt ASSIGNING FIELD-SYMBOL(<ls_booking_det>).

        "-- Requirement #2
        CASE <ls_booking_det>-customer_detail_st-customer_type.
          WHEN lc_customer_types-business_b.
            ADD 1 TO ls_rpt_flight_data-passenger_in_business.
          WHEN lc_customer_types-private_p.
            ADD 1 TO ls_rpt_flight_data-passenger_in_private.
          WHEN OTHERS.
        ENDCASE.

        " Requirement 3: Get Highest/lowest details
        IF ls_rpt_flight_data-lowest_airfare  IS INITIAL OR
           ls_rpt_flight_data-highest_airfare IS INITIAL.

          ls_rpt_flight_data-lowest_airfare = ls_rpt_flight_data-highest_airfare = <ls_booking_det>-local_cost.
        ENDIF.

        IF ls_rpt_flight_data-highest_airfare < <ls_booking_det>-local_cost.
          ls_rpt_flight_data-highest_airfare = <ls_booking_det>-local_cost.
        ELSEIF ls_rpt_flight_data-lowest_airfare > <ls_booking_det>-local_cost.
          ls_rpt_flight_data-lowest_airfare = <ls_booking_det>-local_cost.
        ENDIF.

        " Requirement 4: Get a count of passengers different than the starting location of a flight
        IF <ls_flight_data>-flight_schedule_st-country_from <> <ls_booking_det>-customer_detail_st-country.
          ADD 1 TO ls_rpt_flight_data-passenger_destin_and_home.
        ENDIF.

        " Requirement 5: For a flight, get agent count that has the same zip as flight destination
        IF <ls_booking_det>-travel_agency_st-country = <ls_flight_data>-flight_schedule_st-country_to.
          ADD 1 TO ls_rpt_flight_data-agent_flight_dest.
        ENDIF.
      ENDLOOP.

      " Requirement 6: Gold Star Flight Check
      IF ls_rpt_flight_data-agent_flight_dest > 0 AND
         ls_rpt_flight_data-passenger_destin_and_home > 5 AND
         ls_rpt_flight_data-highest_airfare > lc_highest_airfare.

        ls_rpt_flight_data-is_gold_star_flight = abap_true.
      ENDIF.


      et_flight_data = VALUE #( BASE et_flight_data
                              ( ls_rpt_flight_data ) ).

      CLEAR: ls_rpt_flight_data.
    ENDLOOP.


  ENDMETHOD.

  "-----------------------------------------------------------------------------------------------------
  " *** PRIVATE METHODS ***
  "-----------------------------------------------------------------------------------------------------
  METHOD get_flight_data_from_ecc.
    DATA: lt_flight_model_data        TYPE ltt_flight_model_data_sflight,
          lt_booking_details_sbook    TYPE ltt_booking_detail_sbook,
          lt_flight_schedule_spfli    TYPE ltt_flight_schedule_spfli,
          lt_customer_details_scustom TYPE ltt_customer_detail_scustom,
          lt_travel_agency_stravelag  TYPE ltt_travel_agency_stravelag.

    DATA: ltr_carrier_id    TYPE gttr_carrier_id,
          ltr_connection_id TYPE gttr_connection_id,
          ltr_flight_date   TYPE gttr_flight_date.

    "-- GET NEEDED DATA --
    " We need to gather all of the data up front we want to execute
    SELECT carrid AS carrier_id,
           connid AS connection_id,
           fldate AS flight_date
      FROM sflight
      INTO CORRESPONDING FIELDS OF TABLE @lt_flight_model_data.

    ltr_carrier_id = VALUE #( FOR <ls_flight_model> IN lt_flight_model_data
                              sign   = lc_range_tbl-sign-include
                              option = lc_range_tbl-option-equal_to
                            ( low    = <ls_flight_model>-carrier_id ) ).

    ltr_connection_id = VALUE #( FOR <ls_flight_model> IN lt_flight_model_data
                                 sign   = lc_range_tbl-sign-include
               	                 option = lc_range_tbl-option-equal_to
                               ( low    = <ls_flight_model>-connection_id ) ).

    ltr_flight_date = VALUE #( FOR <ls_flight_model> IN lt_flight_model_data
                               sign   = lc_range_tbl-sign-include
                               option = lc_range_tbl-option-equal_to
                             ( low    = <ls_flight_model>-flight_date ) ).

    " We need to get all supporting data that COULD be used.
    IF lines( lt_flight_model_data ) > 0.

      "--BOOKING DETAILS
      SELECT carrid AS carrier_id,
             connid AS connection_id,
             fldate AS flight_date,
             bookid AS booking_id,
             customid  AS customer_id,
             agencynum AS agency_num,
             loccuram  AS local_cost,
             loccurkey AS local_cost_uom
        FROM sbook
        WHERE carrid IN @ltr_carrier_id AND
              connid IN @ltr_connection_id AND
              fldate IN @ltr_flight_date
        INTO CORRESPONDING FIELDS OF TABLE @lt_booking_details_sbook.

      "-- Flight Schedules
      SELECT carrid AS carrier_id,
             connid AS connection_id,
             countryfr AS country_from,
             countryto AS country_to
        FROM spfli
        WHERE carrid IN @ltr_carrier_id AND
              connid IN @ltr_connection_id
        INTO TABLE @lt_flight_schedule_spfli.

    ENDIF.

    "-- CUSTOMER INFORMATION
    IF lines( lt_booking_details_sbook ) > 0.
      SELECT id       AS customer_id,
             custtype AS customer_type,
             country
        FROM scustom
        FOR ALL ENTRIES IN @lt_booking_details_sbook
        WHERE id = @lt_booking_details_sbook-customer_id
        INTO TABLE @lt_customer_details_scustom.


      SELECT agencynum AS agency_num,
             country
        FROM stravelag
        FOR ALL ENTRIES IN @lt_booking_details_sbook
        WHERE agencynum = @lt_booking_details_sbook-agency_num
        INTO TABLE @lt_travel_agency_stravelag.

    ENDIF.

    "--- FINAL COMPLEX STRUCTURE
    " We want to take the data we selected and move it into a complex structure. This will make reading data
    " slight easier for support since this model will be contained in a single complex structure (rather than
    " several table).
    LOOP AT lt_flight_model_data ASSIGNING FIELD-SYMBOL(<ls_flight_model_data>).

      "--Booking
      <ls_flight_model_data>-booking_detail_tt = VALUE #( FOR <ls_booking> IN lt_booking_details_sbook
                                                          WHERE ( carrier_id    = <ls_flight_model_data>-carrier_id AND
                                                                  connection_id = <ls_flight_model_data>-connection_id )
                                                        ( CORRESPONDING #( <ls_booking> ) ) ).

      LOOP AT <ls_flight_model_data>-booking_detail_tt ASSIGNING FIELD-SYMBOL(<ls_booking_line>).
        <ls_booking_line>-customer_detail_st = VALUE #( lt_customer_details_scustom[ customer_id = <ls_booking_line>-customer_id ] OPTIONAL ).
        <ls_booking_line>-travel_agency_st   = VALUE #( lt_travel_agency_stravelag[ agency_num = <ls_booking_line>-agency_num ] OPTIONAL ).
      ENDLOOP.

      "-- Flight Scheduling
      <ls_flight_model_data>-flight_schedule_st = VALUE #( lt_flight_schedule_spfli[ carrier_id    = <ls_flight_model_data>-carrier_id
                                                                                     connection_id = <ls_flight_model_data>-connection_id ] OPTIONAL ).

    ENDLOOP.

    et_flight_data = lt_flight_model_data.
  ENDMETHOD.
ENDCLASS.
