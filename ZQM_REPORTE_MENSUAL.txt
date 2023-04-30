*&---------------------------------------------------------------------*
*& Report  zmaqr002
*&---------------------------------------------------------------------*
*& Compañia   : ******                                                 *
*& Autor      : ******              Felipe Rivera                      *
*& Usuario    : ******                                                 *
*& Funcional  : ******                                                 *
*& Fecha      : 23/05/2014                                             *
*& Objetivo   : Muestra Reporte de Maquila - Cierre Mensual            *
*&~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
REPORT  zmaqr002.

***************************************************************
* Codigos de colores :                                        *
* Color es un char de 4 posiciones, donde:                    *
*              - char 1 = C (Propiedad de Color)              *
*              - char 2 = Codigo del color (de 0 a 7)         *
*                                0 = Transparente             *
*                                1 = Azul                     *
*                                2 = Gris                     *
*                                3 = Amarillo                 *
*                                4 = Azul/Gris                *
*                                5 = Verde                    *
*                                6 = Rojo                     *
*                                7 = Naranja                  *
*              - char 3 = Intensidad  (0=off, 1=on)           *
*              - char 4 = Inverso     (0=off, 1=on)           *
*                                                             *
***************************************************************
*&---------------------------------------------------------------------*

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*   Definiciónes y Selecciones.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
INCLUDE zmaqr002_top.


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                      COMIENZO DE LA SELECCIÓN                        *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
START-OF-SELECTION.

  PERFORM get_data.
  IF gt_salida IS NOT INITIAL.
    CALL SCREEN 100.
  ELSE.
    MESSAGE text-e01 TYPE 'I'.
  ENDIF.

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                              MODULES                                 *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_ALV'.
  SET TITLEBAR  '100'.

  IF grid_m IS INITIAL.
    PERFORM catalog_salida.
    PERFORM build_layout.
    PERFORM exclude.
    PERFORM create_objects.
    PERFORM display_alv_report.
  ELSE.
    CALL METHOD grid_m->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: v_ucomm       TYPE sy-ucomm.
  DATA: ans           TYPE char1.
  DATA: it_zqme16     TYPE STANDARD TABLE OF zqme16  , ls_zqme16  TYPE zqme16.
  DATA: it_zqme017    TYPE STANDARD TABLE OF zqme017 , ls_zqme017 TYPE zqme017.

  v_ucomm = sy-ucomm.

  CASE v_ucomm.
    WHEN '&OC'.
        CLEAR:  ls_zqme16.
      REFRESH:  it_zqme16.

      LOOP AT gt_salida INTO wa_salida WHERE aflag EQ 'X'.
        ls_zqme16-vbeln        = wa_salida-vbeln.
        ls_zqme16-posnr        = wa_salida-posnr.
        ls_zqme16-matnr        = wa_salida-matnr.
        ls_zqme16-charg        = wa_salida-charg.
        ls_zqme16-contrato     = wa_salida-vbeln_cont.
        ls_zqme16-plan_entrega = wa_salida-vbeln_plan.
        ls_zqme16-camp_001     = wa_salida-camp_001.
        ls_zqme16-camp_002     = wa_salida-camp_002.
        ls_zqme16-camp_003     = wa_salida-camp_003.
        ls_zqme16-camp_004     = wa_salida-camp_004.
        ls_zqme16-camp_005     = wa_salida-camp_005.
        ls_zqme16-camp_006     = wa_salida-camp_006.
        ls_zqme16-camp_007     = wa_salida-camp_007.
        ls_zqme16-camp_008     = wa_salida-camp_008.
        ls_zqme16-camp_009     = wa_salida-camp_009.
        ls_zqme16-camp_010     = wa_salida-camp_010.
        ls_zqme16-camp_011     = wa_salida-camp_011.
        ls_zqme16-camp_012     = wa_salida-camp_012.
        APPEND ls_zqme16 TO it_zqme16.
      ENDLOOP.

      it_zqme017[] = gt_struct[].

* llamamos la función para crear los pedidos.
      CALL FUNCTION 'ZQM011_CREA_OC'
        EXPORTING
          i_agrupar = ''
          i_rep     = 'X'
        TABLES
          t_zqme017 = it_zqme017
          t_zqme16  = it_zqme16.

    WHEN '&OC2'.

      CLEAR:    ls_zqme16.
      REFRESH:  it_zqme16.

      LOOP AT gt_salida INTO wa_salida WHERE aflag EQ 'X'.
        ls_zqme16-vbeln        = wa_salida-vbeln.
        ls_zqme16-posnr        = wa_salida-posnr.
        ls_zqme16-matnr        = wa_salida-matnr.
        ls_zqme16-charg        = wa_salida-charg.
        ls_zqme16-contrato     = wa_salida-vbeln_cont.
        ls_zqme16-plan_entrega = wa_salida-vbeln_plan.
        ls_zqme16-camp_001     = wa_salida-camp_001.
        ls_zqme16-camp_002     = wa_salida-camp_002.
        ls_zqme16-camp_003     = wa_salida-camp_003.
        ls_zqme16-camp_004     = wa_salida-camp_004.
        ls_zqme16-camp_005     = wa_salida-camp_005.
        ls_zqme16-camp_006     = wa_salida-camp_006.
        ls_zqme16-camp_007     = wa_salida-camp_007.
        ls_zqme16-camp_008     = wa_salida-camp_008.
        ls_zqme16-camp_009     = wa_salida-camp_009.
        ls_zqme16-camp_010     = wa_salida-camp_010.
        ls_zqme16-camp_011     = wa_salida-camp_011.
        ls_zqme16-camp_012     = wa_salida-camp_012.
        APPEND ls_zqme16 TO it_zqme16.
      ENDLOOP.

      it_zqme017[] = gt_struct[].

* llamamos la función para crear los pedidos.
      CALL FUNCTION 'ZQM011_CREA_OC'
        EXPORTING
          i_agrupar = 'X'
          i_rep     = 'X'
        TABLES
          t_zqme16  = it_zqme16
          t_zqme017 = it_zqme017.


    WHEN '&ALL'.
      PERFORM select_all_entries CHANGING gt_salida[].
    WHEN '&SAL'.
      PERFORM deselect_all_entries CHANGING gt_salida[].
    WHEN '&BACK' OR '&EXIT'.
      LEAVE TO SCREEN 0.
    WHEN '&CANCEL'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Cancelar'
          text_question         = '¿Seguro que desea abandonar el reporte?'
          text_button_1         = 'Si'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = 'X'
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = ans.

      CASE ans.
        WHEN '1'.
          LEAVE PROGRAM.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                               MÉTODOS                                *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*

*&---------------------------------------------------------------------*
*&      Form  handle_data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING       pr_data_changed
      TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cell   TYPE lvc_s_modi.
  DATA: gs_stbl       TYPE lvc_s_stbl.

  SORT pr_data_changed->mt_mod_cells BY row_id.

  LOOP AT pr_data_changed->mt_mod_cells INTO ls_mod_cell.

    CASE ls_mod_cell-fieldname.                              " Nombre del campo que selecciona
      WHEN 'AFLAG'.
*        PERFORM valida_flag USING ls_mod_cell
*                                  pr_data_changed.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

*|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*| Fila y Columna Estable
*|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gs_stbl-row = 'X'.
  gs_stbl-col = 'X'.

  CALL METHOD grid_m->refresh_table_display
    EXPORTING
      is_stable = gs_stbl.

ENDFORM.                    " handle_data_changed


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING    pe_row_id     TYPE lvc_s_row
      pe_column_id  TYPE lvc_s_col
      pes_row_no    TYPE any.

  DATA: lv_fielname TYPE lvc_s_col-fieldname.
  DATA: lv_index    TYPE lvc_s_row-index.
  DATA: gs_stbl     TYPE lvc_s_stbl.

  lv_fielname = pe_column_id-fieldname.
  lv_index    = pe_row_id-index.

  CLEAR: wa_salida.
  CASE lv_fielname.
    WHEN 'AFLAG'.

      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc IS INITIAL.

        IF wa_salida-aflag IS INITIAL.
          wa_salida-aflag       = 'X'.
          wa_salida-color_line  = 'C311'.
        ELSE.
          CLEAR:  wa_salida-aflag.
          CLEAR:  wa_salida-color_line.
        ENDIF.

        MODIFY gt_salida FROM wa_salida INDEX lv_index.
      ENDIF.

*|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*| Fila y Columna Estable
*|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      gs_stbl-row = 'X'.
      gs_stbl-col = 'X'.

      CALL METHOD grid_m->refresh_table_display
        EXPORTING
          is_stable = gs_stbl.

    WHEN 'VBELN'.
      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc EQ 0 AND wa_salida-vbeln IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD wa_salida-vbeln.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'VBELN_PLAN'.
      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc EQ 0 AND wa_salida-vbeln_plan IS NOT INITIAL.
        SET PARAMETER ID 'SAG' FIELD wa_salida-vbeln_plan.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'VBELN_CONT'.
      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc EQ 0 AND wa_salida-vbeln_cont IS NOT INITIAL.
        SET PARAMETER ID 'CTR' FIELD wa_salida-vbeln_cont.
        CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'CHARG'.
      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc EQ 0 AND wa_salida-charg IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD wa_salida-matnr.
        SET PARAMETER ID 'CHA' FIELD wa_salida-charg.
        CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'EBELN'.
      READ TABLE gt_salida INTO wa_salida INDEX lv_index.
      IF sy-subrc EQ 0 AND wa_salida-ebeln IS NOT INITIAL.
        SET PARAMETER ID 'BES' FIELD wa_salida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*                             SUBRUTINAS                               *
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Extrae de las tablas correspondientes los datos a usar
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lv_indic        TYPE char1.
  DATA: lv_smbln        TYPE mseg-smbln.
  DATA: lv_penal        TYPE dec_16_03_s.
  DATA: lv_ret_day      TYPE numc10.
  DATA: lv_valor        TYPE p DECIMALS 3.
  DATA: lv_totales      TYPE p DECIMALS 3.
  DATA: lv_name         TYPE char30.
  DATA: ls_zqme015      TYPE zqme015.
  DATA: ls_molib        TYPE zmm_molib_001.
  DATA: lv_ebeln        TYPE ekko-ebeln.

  DATA: lt_class        TYPE STANDARD TABLE OF sclass.
  DATA: lt_objectdata   TYPE STANDARD TABLE OF clobjdat.
  DATA: wa_objectdata   TYPE clobjdat.

  DATA: lt_item_hist    TYPE STANDARD TABLE OF bapiekbe.
  DATA: wa_item_hist    TYPE bapiekbe.

  DATA: lt_zqmt010      TYPE STANDARD TABLE OF zqmt010.
  DATA: wa_zqmt010      TYPE zqmt010.

*------------------------- rangos
* Rango Material
  DATA: r_matnr  TYPE RANGE OF matnr,
        wa_matnr LIKE LINE OF  r_matnr.

* Rango Centro
  DATA: r_werks  TYPE RANGE OF werks_d ,
        wa_werks LIKE LINE OF  r_werks.

* Rango Almacén
  DATA: r_lgort  TYPE RANGE OF lgort_d,
        wa_lgort LIKE LINE OF  r_lgort.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~> Elementos y caracteristicas de Inspeccion
  SELECT  *
    INTO  TABLE gt_elem
    FROM  zmaq_dinam_002
    ORDER BY ngrup orden.

  gt_elem_grup[] = gt_elem[].
  DELETE ADJACENT DUPLICATES FROM gt_elem_grup COMPARING ngrup grupo.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~> Tabla zqmt010 para filtrar movimientos en mseg
  SELECT *
    INTO TABLE lt_zqmt010
    FROM zqmt010
    ORDER BY indice.

* creo los rangos para el centro y el material especificos.
  LOOP AT lt_zqmt010 INTO wa_zqmt010.
    wa_matnr-sign   = 'I'.
    wa_matnr-option = 'EQ'.
    wa_matnr-low    = wa_zqmt010-matnr.
    APPEND wa_matnr TO r_matnr.

    wa_werks-sign   = 'I'.
    wa_werks-option = 'EQ'.
    wa_werks-low    = wa_zqmt010-werks.
    APPEND wa_werks TO r_werks.

    wa_lgort-sign   = 'I'.
    wa_lgort-option = 'EQ'.
    wa_lgort-low    = wa_zqmt010-lgort.
    APPEND wa_lgort TO r_lgort.

  ENDLOOP.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Se extraen las entregas que cumplen las condiciones

  SELECT  a~vbeln           " Entrega
          a~wadat_ist       " Fecha movimiento de mercancías real
          a~wadat           " Fecha prevista para movimiento de mercancías
          a~kunnr           " Destinatario de mercancías
          b~wbstk           " Status total de movimiento de mercancías
    INTO  CORRESPONDING FIELDS OF TABLE gt_likp
    FROM  likp AS a
          INNER JOIN vbuk AS b
                  ON b~vbeln EQ a~vbeln
    WHERE a~vbeln      IN  s_vbeln
      AND a~wadat_ist  IN  s_wadat
      AND a~kunnr      IN  s_kunnr
      AND a~lfart      EQ  'ZNL1'
      AND b~wbstk      EQ  'C'.


  IF gt_likp IS NOT INITIAL.

    SELECT  ps~vbeln          " Entrega
            ps~posnr          " Posicion
            ps~matnr          " Material
            ps~charg          " Número de lote
            eg~mblnr          " Número de documento material
            eg~budat_mkpf     " Fecha de contabilización en el documento
      INTO  CORRESPONDING FIELDS OF TABLE gt_salida
      FROM  lips  AS ps
            INNER JOIN mseg AS eg
                    ON eg~vbeln_im    EQ ps~vbeln
                   AND eg~vbelp_im    EQ ps~posnr
                   AND eg~charg       EQ ps~charg
                   AND eg~lgort       EQ ps~lgort
      FOR   ALL ENTRIES IN gt_likp
      WHERE ps~charg    IN s_charg
        AND ps~vbeln    EQ gt_likp-vbeln
        AND eg~bwart    EQ '641'.

*~~> Se obtienen los movimientos 101
    SELECT  ps~vbeln          " Entrega
            ps~posnr          " Posicion
            ps~matnr          " Material
            ps~charg          " Número de lote
            eg~mblnr          " Número de documento material
            eg~budat_mkpf     " Fecha de contabilización en el documento
      INTO  CORRESPONDING FIELDS OF TABLE gt_m101
      FROM  lips  AS ps
            INNER JOIN mseg AS eg
                    ON eg~vbeln_im    EQ ps~vbeln
                   AND eg~vbelp_im    EQ ps~posnr
                   AND eg~charg       EQ ps~charg
                 " AND eg~lgort       EQ ps~lgort
      FOR   ALL ENTRIES IN gt_likp
      WHERE ps~charg    IN s_charg
        AND ps~vbeln    EQ gt_likp-vbeln
        AND eg~bwart    EQ '101'
        AND eg~werks    IN r_werks
        AND eg~lgort    IN r_lgort
        AND eg~matnr    IN r_matnr.


    LOOP AT gt_salida INTO wa_salida.
      CLEAR:  lv_objectkey.
      CLEAR:  lt_class[].
      CLEAR:  lt_objectdata[].
      CLEAR:  wa_objectdata.
      CLEAR:  lv_penal.
      CLEAR:  lv_ret_day.
      CLEAR:  lv_indic.
      CLEAR:  gv_tabix.

      gv_tabix = sy-tabix.

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Valida elimina los que tengan stock en libre utilización.
*        CLEAR: ls_mchbx.
*        SELECT SINGLE * INTO ls_mchbx FROM mchb WHERE charg EQ wa_salida-charg AND
*                                                      matnr EQ wa_salida-matnr AND
*                                                      clabs <> space           .
*        IF ls_mchbx IS NOT INITIAL.
*         DELETE gt_salida INDEX gv_tabix.
*         CONTINUE.
*        ENDIF.

**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Valida si el documento posee anulación. En caso de, se elimina el registro
      SELECT  SINGLE smbln
        INTO  lv_smbln
        FROM  mseg
        WHERE smbln EQ wa_salida-mblnr.

      IF sy-subrc IS INITIAL.
        DELETE gt_salida INDEX gv_tabix.
        lv_indic = 'X'.
      ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> SE VALIDA SI EL REGISTRO ENTRANTE NO FUE ELIMINADO
      IF lv_indic IS INITIAL.     " NE 'X'

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Completa datos en cabecera de entrega
        READ TABLE gt_likp INTO wa_likp WITH KEY vbeln = wa_salida-vbeln.
        wa_salida-wadat_ist = wa_likp-wadat_ist.
        wa_salida-wadat     = wa_likp-wadat.
        wa_salida-kunnr     = wa_likp-kunnr.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Se obtiene el Lote de Proveedor
        SELECT  SINGLE licha
          INTO  wa_salida-licha
          FROM  mch1
          WHERE matnr EQ wa_salida-matnr
            AND charg EQ wa_salida-charg.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Se Obtiene Plan de Entregas y contrato Asociado.
        SELECT  SINGLE ebeln
          INTO  wa_salida-vbeln_plan
          FROM  ekbe
          WHERE belnr EQ wa_salida-vbeln.

        IF sy-subrc IS INITIAL.
          SELECT  SINGLE ihrez
            INTO  wa_salida-vbeln_cont
            FROM  ekko
            WHERE ebeln EQ wa_salida-vbeln_plan.
        ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Provisional Quantity + Numero de BAGS

        CLEAR: lv_bag ,               " Numero de Bags (MaxiSacos)
               lv_gross ,             " Gross Weight (KG)
               lv_tare,               " Tare (KG)
               lv_net  ,              " Net Weight (KG)
               lv_moisture,           " Moisture (%)
               lv_dry.                " Dry Weight (KG)

        CALL FUNCTION 'ZQM012_PESOS'
          EXPORTING
            i_entrega = wa_salida-vbeln
            i_matnr   = wa_salida-matnr
            i_lote    = wa_salida-charg
            i_bwart   = '641'
          IMPORTING
            gross     = lv_gross
            bag       = lv_bag
            tare      = lv_tare
            moisture  = lv_moisture
            net       = lv_net
            dry       = lv_dry.

        wa_salida-nbags = lv_bag.           " Numero de Bags (MaxiSacos)
        wa_salida-gross = lv_gross.         " Gross Weight (KG)
        wa_salida-tarek = lv_tare.          " Tare (KG)
        wa_salida-netwe = lv_net.           " Net Weight (KG)
        wa_salida-moist = lv_moisture.      " Moisture (%)
        wa_salida-drywe = lv_dry.           " Dry Weight (KG)


        CONCATENATE wa_salida-matnr wa_salida-charg INTO lv_objectkey.

        CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
          EXPORTING
            classtext          = 'X'
            classtype          = '023'
            features           = 'X'
            language           = sy-langu
            object             = lv_objectkey
            objecttable        = 'MCH1'
            key_date           = sy-datum
            initial_charact    = 'X'
            change_service_clf = 'X'
          TABLES
            t_class            = lt_class
            t_objectdata       = lt_objectdata
          EXCEPTIONS
            no_classification  = 1
            no_classtypes      = 2
            invalid_class_type = 3
            other              = 4.


        CLEAR: wa_m101.
        READ TABLE gt_m101 INTO wa_m101 WITH KEY vbeln = wa_salida-vbeln
                                                 matnr = wa_salida-matnr
                                                 posnr = wa_salida-posnr
                                                 charg = wa_salida-charg.

        IF sy-subrc IS INITIAL.

          CLEAR:  lv_bag,                 " Numero de Bags (MaxiSacos)
                  lv_gross,               " Gross Weight (KG)
                  lv_tare,                " Tare (KG)
                  lv_net,                 " Net Weight (KG)
                  lv_moisture,            " Moisture (%)
                  lv_dry.                 " Dry Weight (KG)

          CALL FUNCTION 'ZQM012_PESOS'
            EXPORTING
              i_entrega = wa_salida-vbeln
              i_matnr   = wa_salida-matnr
              i_lote    = wa_salida-charg
              i_bwart   = '101'
              i_reporte = 'X'
            IMPORTING
              gross     = lv_gross
              bag       = lv_bag
              tare      = lv_tare
              moisture  = lv_moisture
              net       = lv_net
              dry       = lv_dry
              tipo      = wa_salida-tipo.

          wa_salida-gross2 = lv_gross.              " Gross Weight (KG)
          wa_salida-tarek2 = lv_tare.               " Tare (KG)
          wa_salida-netwe2 = lv_net.                " Net Weight (KG)
          wa_salida-moist2 = lv_moisture.           " Moisture (%)
          wa_salida-drywe2 = lv_dry.                " Dry Weight (KG)

*~~> Se Obtiene de la caracteristica la Fecha CONTRACTUAL DE MOLYMET
          IF lt_objectdata IS NOT INITIAL.
            READ TABLE lt_objectdata INTO wa_objectdata WITH KEY atnam = 'F_RETORNO'.
            IF sy-subrc IS INITIAL AND wa_objectdata-atflv IS NOT INITIAL.

              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external = wa_objectdata-ausp1
                IMPORTING
                  date_internal = wa_salida-wadat_moly.
            ENDIF.
          ENDIF.

        ELSE.
          CLEAR: wa_salida-budat_mkpf.
          CLEAR: wa_salida-wadat_moly.
        ENDIF.
*
 IF wa_salida-budat_mkpf IS NOT INITIAL AND
    wa_salida-gross2     IS INITIAL. "
* descartamos el registro del reporte.
    DELETE gt_salida INDEX gv_tabix.
    CONTINUE.
  ENDIF.

        IF lt_objectdata IS NOT INITIAL.
          CLEAR:  gv_indice.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Analisís de intercambio
          LOOP AT gt_elem_grup INTO wa_elem_grup WHERE ngrup LT 500.

            LOOP AT gt_elem INTO wa_elem WHERE ngrup EQ wa_elem_grup-ngrup.
              ADD 1 TO gv_indice.
              CLEAR: lv_name.


              CONCATENATE 'WA_SALIDA-ELEM_' gv_indice INTO lv_name.
              ASSIGN (lv_name) TO <fs1>.

              READ TABLE lt_objectdata INTO wa_objectdata WITH KEY atnam = wa_elem-carac.
              IF sy-subrc IS INITIAL.
                <fs1> = wa_objectdata-atflv.
              ENDIF.

            ENDLOOP.
          ENDLOOP.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~> CAMPOS
          CLEAR: gv_indice.
          LOOP AT gt_elem_grup INTO wa_elem_grup WHERE ngrup GT 500.
            CLEAR:  lv_totales.

            LOOP AT gt_elem INTO wa_elem WHERE ngrup EQ wa_elem_grup-ngrup.
              ADD 1 TO gv_indice.
              CLEAR: lv_name.
              CLEAR: lv_valor.
              CLEAR: ls_molib.

              CONCATENATE 'WA_SALIDA-CAMP_' gv_indice INTO lv_name.
              ASSIGN (lv_name) TO <fs1>.

              IF wa_elem-carac NE 'TOTAL'.

                READ TABLE lt_objectdata INTO wa_objectdata WITH KEY atnam = wa_elem-carac.
                IF sy-subrc IS INITIAL.

                  lv_valor = wa_objectdata-atflv.

*~~> Se buscan los parametros adicionales en la tabla Z
                  SELECT  SINGLE *
                    INTO  ls_molib
                    FROM  zmm_molib_001
                    WHERE contnr  EQ wa_salida-vbeln_cont
                      AND limin   LE lv_valor
                      AND limsu   GE lv_valor
                      AND ( atna1 EQ wa_elem-carac OR
                            atna2 EQ wa_elem-carac ).

                  IF sy-subrc NE 0.
                    SELECT  SINGLE *
                      INTO  ls_molib
                      FROM  zmm_molib_001
                      WHERE limin  LE lv_valor
                        AND limsu  GE lv_valor
                        AND ( atna1 EQ wa_elem-carac OR
                              atna2 EQ wa_elem-carac ).
                  ENDIF.

                  IF ls_molib IS NOT INITIAL.
                    CASE wa_elem-ngrup.
                      WHEN 501.                   "~~> PENALTY
                        <fs1>      = ls_molib-penal.
                        lv_totales = lv_totales + ls_molib-penal.
                        lv_penal   = lv_penal + ls_molib-penal.
                      WHEN 502.                   "~~> LOSS
                        <fs1>      = ls_molib-pmeta.
                        lv_totales = lv_totales + ls_molib-pmeta.
                      WHEN 503.                   "~~> RETURN
                        <fs1>      = ls_molib-plazo.
                        lv_totales = lv_totales + ls_molib-plazo.
                        lv_ret_day = lv_ret_day + ls_molib-plazo.
                      WHEN 504.                   "~~> VOLUME
                        <fs1>      = ls_molib-vomax.
                        lv_totales = lv_totales + ls_molib-vomax.
                      WHEN OTHERS.
                    ENDCASE.
                  ENDIF.
                ENDIF.

              ELSE.
                <fs1> = lv_totales.
              ENDIF.

            ENDLOOP.
          ENDLOOP.
        ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Datos adicionales.
        CLEAR: ls_zqme015.
        CALL FUNCTION 'ZQM010_EXTRACTOR_DATOS_02'
          EXPORTING
            i_dryw       = wa_salida-drywe2
            i_dryw_um    = 'KG'
            i_vbeln      = wa_salida-vbeln
            i_lote       = wa_salida-charg
            i_posnr      = wa_salida-posnr
            i_matnr      = wa_salida-matnr
            i_penal      = lv_penal
            i_ret_tot    = lv_ret_day
            i_car_fin    = ''
            i_budat_mkpf = wa_salida-budat_mkpf
          IMPORTING
            es_salida    = ls_zqme015.

        IF sy-subrc IS INITIAL.
          wa_salida-content     = ls_zqme015-mo_content.                " MO Content (Kg)
          wa_salida-content_lb  = ls_zqme015-mo_content_lb.             " MO Content (Lib)
          wa_salida-rate_re     = ls_zqme015-recovery_rate.             " Recovery Rate (%)
          wa_salida-returna     = ls_zqme015-mo_returnable.             " Mo returnable (kg)
          wa_salida-returna_lb  = ls_zqme015-mo_returnable * gc_lib.    " Mo returnable (Lib)
          wa_salida-fee_mo      = ls_zqme015-cuota_pro.                 " Processing Fee (US$/lb Mo)
          wa_salida-fee_us      = ls_zqme015-pro_fee.                   " Processing Fee (US$)
          wa_salida-return      = ls_zqme015-days.                      " Return Period (days)
          wa_salida-ret_tot     = ls_zqme015-days_tot.                  "
        ENDIF.

        CONCATENATE wa_salida-budat_mkpf+0(4) wa_salida-budat_mkpf+4(2)
                    INTO wa_salida-arri_month
                    SEPARATED BY '-'.                                   " Date of Arrival MONTH

        IF wa_salida-budat_mkpf IS NOT INITIAL.
          wa_salida-retu_fe = wa_salida-budat_mkpf + wa_salida-ret_tot.   " Date of Return
        ENDIF.
        CONCATENATE wa_salida-retu_fe+0(4) wa_salida-retu_fe+4(2)
                    INTO wa_salida-retu_month
                    SEPARATED BY '-'.                                   " Date of Return MONTH


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Indicador de existencia de Orden de Compra OC (PO)
        IF wa_salida-vbeln_cont IS NOT INITIAL.
          SELECT  SINGLE a~ebeln
            INTO  lv_ebeln
            FROM  ekko AS a
                  INNER JOIN eket AS b
                          ON a~ebeln EQ b~ebeln
                  INNER JOIN ekpo AS o
                          ON o~ebeln EQ b~ebeln
            WHERE a~konnr EQ wa_salida-vbeln_cont
              AND a~loekz EQ space                " Indicador de borrado
              AND b~charg EQ wa_salida-charg.

          IF sy-subrc IS INITIAL.
            CLEAR: lt_item_hist.
            wa_salida-ebeln_ind = '@01@'.
            wa_salida-ebeln     = lv_ebeln.

*~~> Se obtiene el monto total en Facturas
            CALL FUNCTION 'BAPI_PO_GETDETAIL'
              EXPORTING
                purchaseorder   = lv_ebeln
                history         = 'X'
              TABLES
                po_item_history = lt_item_hist.

            IF lt_item_hist IS NOT INITIAL.

              LOOP AT lt_item_hist INTO wa_item_hist WHERE hist_type EQ 'Q'.

                wa_salida-monto_fact = wa_salida-monto_fact + wa_item_hist-val_forcur.
                wa_salida-monto_un   = wa_item_hist-currency.

              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Calculo acumulativo de campo MO DELIVERY
        gv_monto          = gv_monto + wa_salida-content.
        wa_salida-deli_mo = gv_monto.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Almacenando Cambios

        MODIFY gt_salida FROM wa_salida INDEX gv_tabix.

      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  CATALOG_SALIDA_DETA
*&---------------------------------------------------------------------*
*       Construcción del Catalogo para salida ALV
*----------------------------------------------------------------------*
FORM catalog_salida.
  DATA: lv_name   TYPE char30.
  DATA: lv_empha  TYPE char4.
  DATA: lv_fname  TYPE char30.
  DATA: lv_titul  TYPE char30.
  DATA: lv_resto  TYPE i.

*--->>> FLAG
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'AFLAG'.            wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Sel.'.             wa_fcat-tooltip     = 'Selección'.
  wa_fcat-key         = 'X'.                wa_fcat-checkbox    = 'X'.
  wa_fcat-edit        = 'X'.                wa_fcat-hotspot     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> FECHA MOVIMIENTO DE MERCANCÍAS REAL
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'WADAT_IST'.        wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Dispatch Date'.    wa_fcat-tooltip     = 'Dispatch Date'.
  wa_fcat-key         = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> FECHA DE RECEPCION CONTRACTUAL
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'WADAT'.            wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Contractual Date'. wa_fcat-tooltip     = 'Contractual Date'.
  wa_fcat-key         = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> FECHA DE CONTABILIZACIÓN EN EL DOCUMENTO
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'BUDAT_MKPF'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Arrival Date'.     wa_fcat-tooltip     = 'Arrival Date'.
  wa_fcat-key         = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> FECHA DE RECEPCION CONTRACTUAL MOLYMET
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'WADAT_MOLY'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Molymet Cont.Dat'. wa_fcat-tooltip     = 'Molymet Contractual Date'.
  wa_fcat-key         = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> CONTRATO
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'VBELN_CONT'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'N° Contrato'.      wa_fcat-tooltip     = 'Número de Contrato'.
  wa_fcat-key         = 'X'.                wa_fcat-no_zero     = 'X'.
  wa_fcat-hotspot     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> PLAN DE ENTREGA
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'VBELN_PLAN'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Plan de Ent'.      wa_fcat-tooltip     = 'Plan de Entrega'.
  wa_fcat-key         = 'X'.                wa_fcat-no_zero     = 'X'.
  wa_fcat-hotspot     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> ENTREGA
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'VBELN'.            wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Delivery'.         wa_fcat-tooltip     = 'Delivery Number'.
  wa_fcat-key         = 'X'.                wa_fcat-no_zero     = 'X'.
  wa_fcat-hotspot     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> NÚMERO DE LOTE
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'CHARG'.            wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'MLCC Lot'.         wa_fcat-tooltip     = 'Número de Lote MLCC'.
  wa_fcat-no_zero     = 'X'.                wa_fcat-hotspot     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> LOTE DE Proveedor
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'LICHA'.            wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Lot. Prov'.        wa_fcat-tooltip     = 'Lote de Proveedor'.
  wa_fcat-no_zero     = 'X'.
  APPEND wa_fcat TO gt_fcat.

*--->>> NÚMERO DE BAGS
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'NBAGS'.            wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = '# Bag'.            wa_fcat-tooltip     = '# Bag'.
  APPEND wa_fcat TO gt_fcat.

*--->>> GROSS WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'GROSS'.            wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MLCC Gross(Kg)'.   wa_fcat-tooltip     = 'MLCC Gross Weight'.
  wa_fcat-emphasize   = 'C500'.
  APPEND wa_fcat TO gt_fcat.

*--->>> TARE
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'TAREK'.            wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MLCC Tare(Kg)'.    wa_fcat-tooltip     = 'MLCC Tare'.
  wa_fcat-emphasize   = 'C500'.
  APPEND wa_fcat TO gt_fcat.

*--->>> NET WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'NETWE'.              wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MLCC Net Weight(Kg)'.wa_fcat-tooltip     = 'MLCC Net Weight'.
  wa_fcat-emphasize   = 'C500'.
  APPEND wa_fcat TO gt_fcat.

*--->>> MOISTURE
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'MOIST'.              wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MLCC Moisture(%)'.   wa_fcat-tooltip     = 'MLCC Moisture'.
  wa_fcat-emphasize   = 'C500'.
  APPEND wa_fcat TO gt_fcat.

*--->>> DRY WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'DRYWE'.              wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MLCC Dry Weight(Kg)'.wa_fcat-tooltip     = 'MLCC Dry Weight'.
  wa_fcat-emphasize   = 'C500'.
  APPEND wa_fcat TO gt_fcat.


******** mov - 101
*--->>> GROSS WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'GROSS2'.         wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MAQ Gross(Kg)'.  wa_fcat-tooltip     = 'MAQUILADORA Gross Weight'.
  wa_fcat-emphasize   = 'C711'.
  APPEND wa_fcat TO gt_fcat.

*--->>> TARE
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'TAREK2'.         wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MAQ Tare(Kg)'.   wa_fcat-tooltip     = 'MAQUILADORA Tare'.
  wa_fcat-emphasize   = 'C711'.
  APPEND wa_fcat TO gt_fcat.

*--->>> NET WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'NETWE2'.             wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MAQ Net Weight(Kg)'. wa_fcat-tooltip     = 'MAQUILADORA Net Weight'.
  wa_fcat-emphasize   = 'C711'.
  APPEND wa_fcat TO gt_fcat.

*--->>> MOISTURE
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'MOIST2'.           wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MAQ Moisture(%)'.  wa_fcat-tooltip     = 'MAQUILADORA Moisture'.
  wa_fcat-emphasize   = 'C711'.
  APPEND wa_fcat TO gt_fcat.

*--->>> DRY WEIGHT
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'DRYWE2'.             wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MAQ Dry Weight(Kg)'. wa_fcat-tooltip     = 'MAQUILADORA Dry Weight'.
  wa_fcat-emphasize   = 'C711'.
  APPEND wa_fcat TO gt_fcat.

  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'TIPO'.               wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Q/LU'.                  wa_fcat-tooltip     = 'Tipo Stock'.
  wa_fcat-emphasize   = 'C501'.
  APPEND wa_fcat TO gt_fcat.


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~> CAMPOS DE ANALISIS DE INSPECCIÓN
  CLEAR:  gv_indice.
  LOOP AT gt_elem_grup INTO wa_elem_grup WHERE ngrup LT 500.

    lv_name   = wa_elem_grup-grupo.
    lv_resto  = wa_elem_grup-ngrup MOD 2.
    IF lv_resto NE 0.
      lv_empha  = 'C300'.
    ELSE.
      lv_empha  = 'C400'.
    ENDIF.

    LOOP AT gt_elem INTO wa_elem WHERE ngrup EQ wa_elem_grup-ngrup.
      CLEAR:  lv_fname.
      CLEAR:  lv_titul.
      ADD 1 TO gv_indice.

      CONCATENATE 'ELEM_' gv_indice                   INTO lv_fname.
      CONCATENATE wa_elem-elemn '(' wa_elem-unidm ')' INTO lv_titul.
      CONCATENATE lv_name lv_titul                    INTO lv_titul SEPARATED BY space.

      CLEAR: wa_struct.
      wa_struct-campo = lv_fname.
      wa_struct-eleme = wa_elem-elemn.
      wa_struct-carac = wa_elem-carac.
      wa_struct-grupo = wa_elem-grupo.
      wa_struct-desca = wa_elem-desca.
      APPEND wa_struct TO gt_struct.

      CLEAR  wa_fcat.
      wa_fcat-fieldname   = lv_fname.           wa_fcat-just        = 'R'.
      wa_fcat-scrtext_m   = lv_titul.           wa_fcat-tooltip     = lv_titul.
      wa_fcat-emphasize   = lv_empha.
      APPEND wa_fcat TO gt_fcat.

    ENDLOOP.
  ENDLOOP.

  CLEAR:  gv_indice.
  LOOP AT gt_elem_grup INTO wa_elem_grup WHERE ngrup GT 500.

    lv_name   = wa_elem_grup-grupo.
    lv_resto  = wa_elem_grup-ngrup MOD 2.
    IF lv_resto NE 0.
      lv_empha  = 'C500'.
    ELSE.
      lv_empha  = 'C700'.
    ENDIF.

    LOOP AT gt_elem INTO wa_elem WHERE ngrup EQ wa_elem_grup-ngrup.
      CLEAR:  lv_fname.
      CLEAR:  lv_titul.
      ADD 1 TO gv_indice.

      CONCATENATE 'CAMP_' gv_indice                   INTO lv_fname.
      CONCATENATE wa_elem-elemn '(' wa_elem-unidm ')' INTO lv_titul.
      CONCATENATE lv_name lv_titul                    INTO lv_titul SEPARATED BY space.

      CLEAR: wa_struct.
      wa_struct-campo = lv_fname.
      wa_struct-eleme = wa_elem-elemn.
      wa_struct-carac = wa_elem-carac.
      wa_struct-grupo = wa_elem-grupo.
      wa_struct-desca = wa_elem-desca.
      APPEND wa_struct TO gt_struct.

      CLEAR  wa_fcat.
      wa_fcat-fieldname   = lv_fname.           wa_fcat-just        = 'R'.
      wa_fcat-scrtext_m   = lv_titul.           wa_fcat-tooltip     = lv_titul.
      wa_fcat-emphasize   = lv_empha.
      IF wa_elem_grup-ngrup EQ 503.
        wa_fcat-decimals_o  = 0.
      ENDIF.

      APPEND wa_fcat TO gt_fcat.

    ENDLOOP.
  ENDLOOP.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*--->>> MO Content (Kg)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'CONTENT'.        wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MO Content (Kg)'.wa_fcat-tooltip     = 'MO Content (Kg)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> MO Content (Lib)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'CONTENT_LB'.       wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'MO Content (Lib)'. wa_fcat-tooltip     = 'MO Content (Lib)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Recovery Rate (%)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RATE_RE'.          wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Recovery Rate(%)'. wa_fcat-tooltip     = 'Recovery Rate (%)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Mo RETURNABLE(kg)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RETURNA'.          wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Mo returnable(kg)'.wa_fcat-tooltip     = 'Mo returnable(kg)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Mo RETURNABLE(LIB)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RETURNA_LB'.         wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Mo returnable(Lb)'.  wa_fcat-tooltip     = 'Mo returnable(Lb)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Processing Fee (US$/lb Mo)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'FEE_MO'.               wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Proc. Fee(US$/lb Mo)'. wa_fcat-tooltip     = 'Processing Fee (US$/lb Mo)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Processing Fee (US$)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'FEE_US'.           wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Proc. Fee (US$)'.  wa_fcat-tooltip     = 'Processing Fee (US$)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Return Period (days)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RETURN'.         wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Return Period'.  wa_fcat-tooltip     = 'Return Period (days)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Return Period TOTAL(days)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RET_TOT'.        wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Return TOTAL'.   wa_fcat-tooltip     = 'Return Period TOTAL (days)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Mo delivery accumlation (kg)
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'DELI_MO'.              wa_fcat-just        = 'R'.
  wa_fcat-scrtext_m   = 'Mo Delivery Accum(kg)'.wa_fcat-tooltip     = 'Mo Delivery Accum (kg)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Date of Return
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RETU_FE'.        wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Date of Return'. wa_fcat-tooltip     = 'Date of Return'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Date of Arrival MONTH
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'ARRI_MONTH'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Month of Arrival'. wa_fcat-tooltip     = 'Date of Arrival (MONTH)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> Date of Return MONTH
  CLEAR  wa_fcat.
  wa_fcat-fieldname   = 'RETU_MONTH'.       wa_fcat-just        = 'C'.
  wa_fcat-scrtext_m   = 'Month of Return'.  wa_fcat-tooltip     = 'Date of Return (MONTH)'.
  APPEND wa_fcat TO gt_fcat.

*--->>> PURCHASE ORDER Validation
*  CLEAR  wa_fcat.
*  wa_fcat-fieldname   = 'EBELN_IND'.        wa_fcat-just        = 'C'.
*  wa_fcat-scrtext_m   = 'PO'.               wa_fcat-tooltip     = 'Purchase Order'.
*  wa_fcat-emphasize   = 'C311'.
*  APPEND wa_fcat TO gt_fcat.

*--->>> PURCHASE ORDER
*  CLEAR  wa_fcat.
*  wa_fcat-fieldname   = 'EBELN'.            wa_fcat-just        = 'C'.
*  wa_fcat-scrtext_m   = 'PO Number'.        wa_fcat-tooltip     = 'Purchase Order Number'.
*  wa_fcat-no_zero     = 'X'.                wa_fcat-hotspot     = 'X'.
*  APPEND wa_fcat TO gt_fcat.
*
**--->>> MONTO FACTURADO
*  CLEAR  wa_fcat.
*  wa_fcat-fieldname   = 'MONTO_FACT'.       wa_fcat-just        = 'R'.
*  wa_fcat-scrtext_m   = 'Amount (Billed)'.  wa_fcat-tooltip     = 'Amount (Billed)'.
*  APPEND wa_fcat TO gt_fcat.
*
**--->>> MONTO FACTURADO CURRENCY
*  CLEAR  wa_fcat.
*  wa_fcat-fieldname   = 'MONTO_UN'.         wa_fcat-just        = 'C'.
*  wa_fcat-scrtext_m   = 'Cur'.              wa_fcat-tooltip     = 'Currency'.
*  APPEND wa_fcat TO gt_fcat.


ENDFORM.                    "catalog_salida


*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Define el layout para el ALV
*----------------------------------------------------------------------*
FORM build_layout.

  gd_layout-zebra           = 'X'.              " Rayado Zebra entre lineas del ALV
  gd_layout-cwidth_opt      = 'X'.              " Optimiza el Ancho de Las columnas
  gd_layout-no_rowmark      = 'X'.              " Elimina la columna de Selección de Filas
  gd_layout-grid_title      = 'Detalles'.       " Titulo Secundario
  gd_layout-info_fname      = 'COLOR_LINE'.     " Campo que Identifica el Color en las Lineas
  gd_layout-ctab_fname      = 'COLOR_CELL'.     " Campo que Identifica el Color en las COlumnas

ENDFORM.                    " build_layout


*&---------------------------------------------------------------------*
*&      Form  exclude
*&---------------------------------------------------------------------*
*       Oculta los botones inutilizados de la Barra de herramientas ALV
*----------------------------------------------------------------------*
FORM exclude.
  DATA wa_exclude TYPE ui_func.
  REFRESH gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_views.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wa_exclude TO gd_exclude.

  CLEAR wa_exclude.
  wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wa_exclude TO gd_exclude.

ENDFORM. " EXCLUDE


*&---------------------------------------------------------------------*
*&      Form  create_objects
*&---------------------------------------------------------------------*
*       Creación de contenedores para Salida ALV
*----------------------------------------------------------------------*
FORM create_objects.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*~~> Se crea el Grid en el contenedor
  CREATE OBJECT grid_m
    EXPORTING
      i_parent = custom_container.

ENDFORM.                    "create_objects


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       Genera Salida ALV
*----------------------------------------------------------------------*
FORM display_alv_report .
  DATA: l_variant TYPE disvariant.

  l_variant-report  = sy-repid.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*--> Llamado al ALV se salida
  CALL METHOD grid_m->set_table_for_first_display
    EXPORTING
      is_layout                     = gd_layout
      is_variant                    = l_variant
      it_toolbar_excluding          = gd_exclude
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_salida
      it_fieldcatalog               = gt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*--> Se crean los eventos
  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->handle_hotspot_click FOR grid_m.
  SET HANDLER event_receiver->handle_user_command  FOR grid_m.
  SET HANDLER event_receiver->handle_double_click  FOR grid_m.
  SET HANDLER event_receiver->handle_data_changed  FOR grid_m.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*--> Registra el evento enter
  CALL METHOD grid_m->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDFORM.                    "display_alv_report


*&---------------------------------------------------------------------*
*&      Form  select_all_entries
*&---------------------------------------------------------------------*
*       Selecciona todos los flags en la salida ALV
*----------------------------------------------------------------------*
FORM select_all_entries CHANGING pt_salida TYPE STANDARD TABLE.
  DATA: ls_salida TYPE ty_salida.
  DATA: l_valid   TYPE c.

  CALL METHOD grid_m->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.
    LOOP AT pt_salida INTO ls_salida.
      ls_salida-aflag       = 'X'.
      ls_salida-color_line  = 'C311'.
      MODIFY pt_salida FROM ls_salida.
    ENDLOOP.
  ENDIF.
  CALL METHOD grid_m->refresh_table_display.

ENDFORM.                               " select_all_entries


*&---------------------------------------------------------------------*
*&      Form  deselect_all_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM deselect_all_entries CHANGING pt_salida TYPE STANDARD TABLE.
  DATA: ls_salida TYPE ty_salida.
  DATA: l_valid   TYPE c.

  CALL METHOD grid_m->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.
    LOOP AT pt_salida INTO ls_salida.
      CLEAR ls_salida-aflag.
      CLEAR ls_salida-color_line.
      MODIFY pt_salida FROM ls_salida.
    ENDLOOP.
  ENDIF.

  CALL METHOD grid_m->refresh_table_display.

ENDFORM.                    "deselect_all_entries
