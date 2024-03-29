FUNCTION-POOL zpp_ws_nucleos.               "MESSAGE-ID ..

CONSTANTS: c_em TYPE tc50d-costr VALUE 'PI_PROD',  "& Entrada de Material
           c_cm TYPE tc50d-costr VALUE 'PI_CONS',  "& Consumo de Material
           c_hs TYPE tc50d-costr VALUE 'PI_PHCON'. "& Hoja de Salario

TYPES: BEGIN OF ty_buscar,
         crid  TYPE coch-crid,
         werk  TYPE coch-werk,
         lgort TYPE afpo-lgort,
         aufpl TYPE afko-aufpl,
         gmein TYPE afko-gmein,
         vornr TYPE afvc-vornr,
         aplzl TYPE afvc-aplzl,
         ftno  TYPE cofv-ftno,
         atwrt TYPE cofv-atwrt,
       END OF ty_buscar,

       BEGIN OF ty_afvc,
         vornr TYPE afvc-vornr,
         aplzl TYPE afvc-aplzl,
       END OF ty_afvc,

       BEGIN OF ty_afvv,
         meinh TYPE afvv-meinh,
         bmsch TYPE afvv-bmsch,
         vge01 TYPE afvv-vge01,
         vgw01 TYPE afvv-vgw01,
         vge02 TYPE afvv-vge02,
         vgw02 TYPE afvv-vgw02,
         vge03 TYPE afvv-vge03,
         vgw03 TYPE afvv-vgw03,
         vge04 TYPE afvv-vge04,
         vgw04 TYPE afvv-vgw04,
         vge05 TYPE afvv-vge05,
         vgw05 TYPE afvv-vgw05,
         vge06 TYPE afvv-vge06,
         vgw06 TYPE afvv-vgw06,
       END OF ty_afvv,

       BEGIN OF ty_atributos,
         clase    TYPE coft-costr,
         atributo TYPE c LENGTH 30,
         valor    TYPE cofv-atwrt,
       END OF ty_atributos.



DATA: it_afvc TYPE TABLE OF ty_afvc,
      wa_afvc TYPE ty_afvc,
      it_afvv TYPE TABLE OF ty_afvv,
      wa_afvv TYPE ty_afvv.

DATA: it_atributos TYPE TABLE OF ty_atributos,
      wa_atributos TYPE ty_atributos.

DATA: wa_retorno TYPE zppe_nucleo_ret,
      wa_notif   TYPE zppe_nucleo_notif.

DATA: wa_buscar    TYPE ty_buscar.

*Tablas Internas
DATA: it_procmessheader       TYPE TABLE OF bapi_rcomhapi,
      it_procmesscharac       TYPE TABLE OF bapi_rcomeapi,
      it_procmessheaderreturn TYPE TABLE OF bapi_rcomhrtc,
      it_procmesscharacreturn TYPE TABLE OF bapi_rcomertc,
      it_return               TYPE TABLE OF bapiret2.

*Areas de Trabajo
DATA: wa_procmessheader       TYPE bapi_rcomhapi,
      wa_procmesscharac       TYPE bapi_rcomeapi,
      wa_procmessheaderreturn TYPE bapi_rcomhrtc,
      wa_procmesscharacreturn TYPE bapi_rcomertc,
      wa_return               TYPE bapiret2.

DATA: it_log                  TYPE TABLE OF zpp_log_ws,
      wa_log                  TYPE zpp_log_ws.

DATA: v_cuenta                TYPE i,
      v_linea                 TYPE i,
      v_numop                 TYPE coch-bid,
      v_num_op                TYPE coch-bid,
      v_atinn                 TYPE cofv-atinn,
      v_atinn_c               TYPE c LENGTH 30.

*Variables Globales
DATA: v_msid TYPE comh-msid,
      v_cantidad_base TYPE afpo-psmng.

*&---------------------------------------------------------------------*
*&      Form  get_sender_name
*&---------------------------------------------------------------------*
*       Obtiene el Receptor
*----------------------------------------------------------------------*
*      -->I_NOMBRE_MENSAJE  Nombre del Mensaje
*      -->C_SENDER_NAME     Receptor
*----------------------------------------------------------------------*
FORM get_sender_name USING i_nombre_mensaje TYPE tc50d-costr
                  CHANGING c_sender_name TYPE tc51-dsadr.

  DATA vl_csdes TYPE tc50d-csdes.

* Rescatamos el Receptor
  SELECT SINGLE csdes INTO vl_csdes
    FROM tc50d
      WHERE costr EQ i_nombre_mensaje.
  IF sy-subrc EQ 0.
    CLEAR c_sender_name.
    SELECT SINGLE dsadr INTO c_sender_name
      FROM tc51
        WHERE csdes EQ vl_csdes.
  ENDIF.
ENDFORM.                    "get_sender_name

*&---------------------------------------------------------------------*
*&      Form  get_centro
*&---------------------------------------------------------------------*
*       Obtiene el Centro
*----------------------------------------------------------------------*
*      -->I_ORDEN    Número de Orden
*      -->C_CENTRO   Centro
*----------------------------------------------------------------------*
FORM get_centro USING i_orden
             CHANGING c_centro TYPE caufv-werks.

  DATA vl_aufnr TYPE caufv-aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_orden
    IMPORTING
      output = vl_aufnr.

* sacamos el centro
  SELECT SINGLE werks INTO c_centro
    FROM caufv
      WHERE aufnr EQ vl_aufnr.
ENDFORM.                    "get_centro

*&---------------------------------------------------------------------*
*&      Form  get_tipo_caract
*&---------------------------------------------------------------------*
*       Obtiene el Tipo de la Característica
*----------------------------------------------------------------------*
*      -->I_CARACT   Característica
*      -->C_TIPO     Tipo
*----------------------------------------------------------------------*
FORM get_tipo_caract USING i_caract TYPE atnam
                  CHANGING c_tipo TYPE cabn-atfor.
  DATA vl_atinn TYPE cabn-atinn.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = i_caract
    IMPORTING
      output = vl_atinn.

  SELECT SINGLE atfor INTO c_tipo
    FROM cabn
      WHERE atinn EQ vl_atinn.
ENDFORM.                    "get_tipo_caract

*&---------------------------------------------------------------------*
*&      Form  formatea_orden
*&---------------------------------------------------------------------*
FORM formatea_orden CHANGING c_num_orden.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_num_orden
    IMPORTING
      output = c_num_orden.
ENDFORM.                    "formatea_orden

*&---------------------------------------------------------------------*
*&      Form  convertir_atinn
*&---------------------------------------------------------------------*
FORM convertir_atinn USING u_input CHANGING c_atinn TYPE cofv-atinn.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = u_input
    IMPORTING
      output = c_atinn.
ENDFORM.                    "convertir_atinn

*&---------------------------------------------------------------------*
*&      Form  obtener_datos_orden
*&---------------------------------------------------------------------*
FORM obtener_datos_orden USING u_orden TYPE coch-bid.
  DATA BEGIN OF ti_coft OCCURS 0.
          INCLUDE STRUCTURE coft.
  DATA END OF ti_coft.

  DATA BEGIN OF ti_cofv OCCURS 0.
          INCLUDE STRUCTURE cofv  .
  DATA END OF ti_cofv.

  DATA BEGIN OF wa_cofv.
          INCLUDE STRUCTURE cofv  .
  DATA END OF wa_cofv.

  DATA: vl_crid TYPE coch-crid,
        vl_werk TYPE coch-werk.

*  SELECT SINGLE crid werk
*    FROM coch
*    INTO (vl_crid,
*          vl_werk)
*    WHERE bid EQ v_numop.
*
*  SELECT * INTO TABLE ti_coft
*    FROM coft
*      WHERE crid EQ vl_crid.
*
*  SELECT * INTO TABLE ti_cofv
*    FROM cofv
*      WHERE crid EQ vl_crid.

* Mod : frivera 29.07.2014 : Modifica la busqueda de la Operación y la Fase.
  DATA: ls_afko TYPE afko.
  DATA: it_afft TYPE STANDARD TABLE OF afft , wa_afft TYPE afft.
  DATA: it_affv TYPE STANDARD TABLE OF affv , wa_affv TYPE affv.

 CLEAR: ls_afko.
 SELECT SINGLE * INTO ls_afko FROM afko WHERE aufnr EQ v_numop.

 SELECT * INTO TABLE it_afft
   FROM  afft
   WHERE aufpl EQ ls_afko-aufpl.

 SELECT * INTO TABLE it_affv
   FROM  affv
  FOR ALL ENTRIES IN it_afft
  WHERE  aufpl EQ it_afft-aufpl AND
         aftzl EQ it_afft-aftzl.

* Fin.Mod.
*  CLEAR it_atributos[].
*  LOOP AT ti_coft.
*    LOOP AT ti_cofv WHERE ftno EQ ti_coft-ftno.
*      CLEAR wa_atributos.
*
*      CLEAR v_atinn.
*      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
*        EXPORTING
*          input  = ti_cofv-atinn
*        IMPORTING
*          output = v_atinn_c.
*
*
*      wa_atributos-clase    = ti_coft-costr.
*      wa_atributos-atributo = v_atinn_c.
*      wa_atributos-valor    = ti_cofv-atwrt.
*      APPEND wa_atributos TO it_atributos.
*
*    ENDLOOP.
*  ENDLOOP.
* Mod. Frivera 29.07.2014

  LOOP AT it_afft INTO wa_afft.
    LOOP At it_affv INTO wa_affv WHERE aufpl EQ wa_afft-aufpl AND
                                       aftzl EQ wa_afft-aftzl .

      CLEAR v_atinn_c.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          input  = wa_affv-atinn
        IMPORTING
          output = v_atinn_c.

       wa_atributos-clase    = wa_afft-costr.
       wa_atributos-atributo = v_atinn_c.
       wa_atributos-atributo = wa_affv-atwrt.

       APPEND wa_atributos TO it_atributos.

    ENDLOOP.
  ENDLOOP.
* Fin.Mod.

ENDFORM.                    "obtener_datos_orden


*&---------------------------------------------------------------------*
*&      Form  cargar_caract
*&---------------------------------------------------------------------*
FORM cargar_caract USING u_mesid      TYPE bapi_rcomeapi-proc_mess_id_tmp
                         u_name_char  TYPE bapi_rcomeapi-name_char
                         u_char_value." TYPE bapi_rcomeapi-char_value.

  DATA it_result TYPE match_result_tab.

  CLEAR wa_procmesscharac.
  wa_procmesscharac-proc_mess_id_tmp = u_mesid.
  wa_procmesscharac-name_char        = u_name_char.

  FIND FIRST OCCURRENCE OF 'DATE' IN u_name_char RESULTS it_result.
  IF sy-subrc EQ 0.
    CONCATENATE u_char_value+4(4)
                u_char_value+2(2)
                u_char_value+0(2)
                INTO wa_procmesscharac-char_value.
  ELSE.
    wa_procmesscharac-char_value = u_char_value.
  ENDIF.

  PERFORM get_tipo_caract USING wa_procmesscharac-name_char CHANGING wa_procmesscharac-data_type.
  APPEND wa_procmesscharac TO it_procmesscharac.
ENDFORM.                    "cargar_caracteristica

*&---------------------------------------------------------------------*
*&      Form  ejecutar_bapi
*&---------------------------------------------------------------------*
FORM ejecutar_bapi TABLES t_retorno TYPE zpptt_nucleo_ret.
  DATA wa_retorno TYPE zppe_nucleo_ret.

  CALL FUNCTION 'BAPI_PROCESS_MESSAGE_CREATEMLT'
    TABLES
      procmessheader       = it_procmessheader
      procmesscharac       = it_procmesscharac
      procmessheaderreturn = it_procmessheaderreturn
      procmesscharacreturn = it_procmesscharacreturn
      return               = it_return.

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR wa_retorno.
    wa_retorno-type = 'S'.
    wa_retorno-number = '000'.
    wa_retorno-message = 'SAP=>OK'.
    APPEND wa_retorno TO t_retorno.
  ENDIF.
ENDFORM.                    "ejecutar_bapi


*&---------------------------------------------------------------------*
*&      Form  entrada_material
*&---------------------------------------------------------------------*
*     Linea 1 => Entrada de Mercancías y Hoja de Salario
*     Entrada de Material
*     PI_PROD
*----------------------------------------------------------------------*
FORM entrada_material.
  DATA: vl_lgort     TYPE afpo-lgort,
        vl_material  LIKE wa_atributos-valor,
        vl_operation LIKE wa_atributos-valor,
        vl_phase     LIKE wa_atributos-valor,
        vl_unit      LIKE wa_atributos-valor,
        vl_batch     LIKE wa_atributos-valor,
        vl_peso_real LIKE wa_atributos-valor,
        vl_peso_aux  LIKE marc-bstfe,
        vl_werks     TYPE werks_d.


  CLEAR: it_procmessheader[],
         it_procmesscharac[],
         it_procmessheaderreturn[],
         it_procmesscharacreturn[].

  CLEAR wa_buscar.
  PERFORM obtener_datos_orden USING v_numop.
  CLEAR wa_procmessheader.
  PERFORM get_sender_name USING c_em CHANGING wa_procmessheader-sender_name.
  wa_procmessheader-proc_mess_id_tmp   = v_msid.
  PERFORM get_centro USING wa_notif-num_op CHANGING wa_procmessheader-plant.
  wa_procmessheader-proc_mess_category = c_em.
  wa_procmessheader-test_flag          = ''.
  APPEND wa_procmessheader TO it_procmessheader.

  CLEAR vl_lgort.
  SELECT SINGLE lgort
    FROM afpo
    INTO vl_lgort
    WHERE aufnr EQ v_num_op.

  CLEAR: vl_material,
         vl_operation,
         vl_phase,
         vl_unit,
         vl_batch,
         vl_peso_real,
         vl_werks,
         vl_peso_aux.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_MATERIAL'.
  IF sy-subrc EQ 0.
    PERFORM formatear_material USING wa_atributos-valor CHANGING vl_material.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_OPERATION'.
  IF sy-subrc EQ 0.
    vl_operation = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_PHASE'.
  IF sy-subrc EQ 0.
    vl_phase = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_UNIT_OF_MEASURE'.
  IF sy-subrc EQ 0.
    vl_unit = wa_atributos-valor.

    SELECT SINGLE meins
      INTO vl_unit
      FROM mara
      WHERE matnr EQ vl_material.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_BATCH'.
  IF sy-subrc EQ 0.
    vl_batch = wa_atributos-valor.
  ENDIF.

  PERFORM get_centro USING v_numop CHANGING vl_werks.

  SELECT SINGLE bstfe
    FROM marc
    INTO vl_peso_aux
    WHERE werks EQ vl_werks
      AND matnr EQ vl_material.

  WRITE vl_peso_aux TO vl_peso_real UNIT vl_unit.
  REPLACE ALL OCCURRENCES OF '.' IN vl_peso_real WITH ','.
  CONDENSE vl_peso_real.

  PERFORM cargar_caract: USING v_msid 'PPPI_PROCESS_ORDER'     v_numop,
                         USING v_msid 'PPPI_EVENT_DATE'        wa_notif-fecha_inicio,
                         USING v_msid 'PPPI_EVENT_TIME'        wa_notif-hora_inicio,
                         USING v_msid 'PPPI_OPERATION'         vl_operation,
                         USING v_msid 'PPPI_PHASE'             vl_phase,
                         USING v_msid 'PPPI_MATERIAL'          vl_material,
                         USING v_msid 'PPPI_MATERIAL_PRODUCED' vl_peso_real,
                         USING v_msid 'PPPI_UNIT_OF_MEASURE'   vl_unit,
                         USING v_msid 'PPPI_BATCH'             vl_batch,
                         USING v_msid 'PPPI_STORAGE_LOCATION'  vl_lgort,
                         USING v_msid 'PPPI_DELIVERY_COMPLETE' '',
                         USING v_msid 'PPPI_STOCK_TYPE'        ''.
ENDFORM.                    "entrada_material
*&---------------------------------------------------------------------*
*&      Form  hoja_salario
*&---------------------------------------------------------------------*
*     Linea 1 => Entrada de Mercancías y Hoja de Salario
*     Hoja de Salario
*     PI_PHCON
*----------------------------------------------------------------------*
FORM hoja_salario.
  DATA: vl_operation LIKE wa_atributos-valor,
        vl_phase     LIKE wa_atributos-valor,
        vl_unit      LIKE wa_atributos-valor,
        vl_phase_res LIKE wa_atributos-valor,
        vl_peso_real LIKE wa_atributos-valor,
        vl_activ1    LIKE wa_atributos-valor,
        vl_activ2    LIKE wa_atributos-valor,
        vl_activ3    LIKE wa_atributos-valor,
        vl_activ4    LIKE wa_atributos-valor,
        vl_activ5    LIKE wa_atributos-valor,
        vl_activ6    LIKE wa_atributos-valor,
        vl_activ1r   LIKE wa_atributos-valor,
        vl_activ2r   LIKE wa_atributos-valor,
        vl_activ3r   LIKE wa_atributos-valor,
        vl_activ4r   LIKE wa_atributos-valor,
        vl_activ5r   LIKE wa_atributos-valor,
        vl_activ6r   LIKE wa_atributos-valor,
        vl_activ1_un LIKE wa_atributos-valor,
        vl_activ2_un LIKE wa_atributos-valor,
        vl_activ3_un LIKE wa_atributos-valor,
        vl_activ4_un LIKE wa_atributos-valor,
        vl_activ5_un LIKE wa_atributos-valor,
        vl_activ6_un LIKE wa_atributos-valor,
        vl_cant_base LIKE wa_atributos-valor,
        vl_werks     TYPE werks_d,
        vl_peso_aux  LIKE marc-bstfe.

  DATA  vl_material  LIKE wa_atributos-valor.

  CLEAR: it_procmessheader[],
         it_procmesscharac[],
         it_procmessheaderreturn[],
         it_procmesscharacreturn[].

  CLEAR wa_buscar.
  CLEAR wa_procmessheader.
  PERFORM get_sender_name USING c_hs CHANGING wa_procmessheader-sender_name.
  wa_procmessheader-proc_mess_id_tmp   = v_msid.
  PERFORM get_centro USING wa_notif-num_op CHANGING wa_procmessheader-plant.
  wa_procmessheader-proc_mess_category = c_hs.
  wa_procmessheader-test_flag          = ''.
  APPEND wa_procmessheader TO it_procmessheader.

*****
  CLEAR: vl_operation,
         vl_phase,
         vl_unit,
         vl_phase_res,
         vl_peso_real,
         vl_activ1,
         vl_activ2,
         vl_activ3,
         vl_activ4,
         vl_activ5,
         vl_activ6,
         vl_activ1_un,
         vl_activ2_un,
         vl_activ3_un,
         vl_activ4_un,
         vl_activ5_un,
         vl_activ6_un,
         vl_werks.

  CLEAR vl_material.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_MATERIAL'.
  IF sy-subrc EQ 0.
    PERFORM formatear_material USING wa_atributos-valor CHANGING vl_material.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_OPERATION'.
  IF sy-subrc EQ 0.
    vl_operation = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_PHASE'.
  IF sy-subrc EQ 0.
    vl_phase = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_UNIT_OF_MEASURE'.
  IF sy-subrc EQ 0.
    vl_unit = wa_atributos-valor.

    SELECT SINGLE meins
      INTO vl_unit
      FROM mara
      WHERE matnr EQ vl_material.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_PHASE_RESOURCE'.
  IF sy-subrc EQ 0.
    vl_phase_res = wa_atributos-valor.
  ENDIF.

*  vl_peso_real = wa_notif-peso_real.
  SELECT SINGLE bstfe
    FROM marc
    INTO vl_peso_aux
    WHERE werks EQ vl_werks
      AND matnr EQ vl_material.
  WRITE vl_peso_aux TO vl_peso_real UNIT vl_unit.
  REPLACE ALL OCCURRENCES OF '.' IN vl_peso_real WITH ','.
  CONDENSE vl_peso_real.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_MATERIAL_QUANTITY'.
  IF sy-subrc EQ 0.
    vl_cant_base = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_1'.
  IF sy-subrc EQ 0.
    vl_activ1 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_2'.
  IF sy-subrc EQ 0.
    vl_activ2 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_3'.
  IF sy-subrc EQ 0.
    vl_activ3 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_4'.
  IF sy-subrc EQ 0.
    vl_activ4 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_5'.
  IF sy-subrc EQ 0.
    vl_activ5 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_6'.
  IF sy-subrc EQ 0.
    vl_activ6 = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_1_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ1_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_2_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ2_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_3_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ3_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_4_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ4_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_5_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ5_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000003'
                                                     atributo = 'PPPI_ACTIVITY_6_UNIT'.
  IF sy-subrc EQ 0.
    vl_activ6_un = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_MATERIAL_QUANTITY'.
  IF sy-subrc EQ 0.
    vl_cant_base = wa_atributos-valor.
  ENDIF.

  vl_activ1r = 0.
  vl_activ2r = 0.
  vl_activ3r = 0.
  vl_activ4r = 0.
  vl_activ5r = 0.
  vl_activ6r = 0.

  IF vl_activ1 NE 0.
    vl_activ1r = wa_notif-peso_real * vl_cant_base / vl_activ1.
  ENDIF.
  IF vl_activ2 NE 0.
    vl_activ2r = wa_notif-peso_real * vl_cant_base / vl_activ2.
  ENDIF.
  IF vl_activ3 NE 0.
    vl_activ3r = wa_notif-peso_real * vl_cant_base / vl_activ3.
  ENDIF.
  IF vl_activ4 NE 0.
    vl_activ4r = wa_notif-peso_real * vl_cant_base / vl_activ4.
  ENDIF.
  IF vl_activ5 NE 0.
    vl_activ5r = wa_notif-peso_real * vl_cant_base / vl_activ5.
  ENDIF.
  IF vl_activ6 NE 0.
    vl_activ6r = wa_notif-peso_real * vl_cant_base / vl_activ6.
  ENDIF.

  PERFORM cargar_caract: USING v_msid 'PPPI_PROCESS_ORDER'           v_numop,
                         USING v_msid 'PPPI_EVENT_DATE'              wa_notif-fecha_inicio,
                         USING v_msid 'PPPI_EVENT_TIME'              wa_notif-hora_inicio,
                         USING v_msid 'PPPI_OPERATION'               vl_operation,
                         USING v_msid 'PPPI_PHASE'                   vl_phase,
                         USING v_msid 'PPPI_UNIT_OF_MEASURE'         vl_unit,
                         USING v_msid 'PPPI_PHASE_RESOURCE'          vl_phase_res,
                         USING v_msid 'PPPI_YIELD_TO_CONFIRM'        vl_peso_real,
                         USING v_msid 'PPPI_CONFIRMATION_SHORT_TEXT' '',
                         USING v_msid 'PPPI_PLANT_OF_RESOURCE'       '',
                         USING v_msid 'PPPI_STATUS_CONFIRMED'        '00004',
                         USING v_msid 'PPPI_ACTIVITY_1'              vl_activ1r,
                         USING v_msid 'PPPI_ACTIVITY_2'              vl_activ2r,
                         USING v_msid 'PPPI_ACTIVITY_3'              vl_activ3r,
                         USING v_msid 'PPPI_ACTIVITY_4'              vl_activ4r,
                         USING v_msid 'PPPI_ACTIVITY_5'              vl_activ5r,
                         USING v_msid 'PPPI_ACTIVITY_6'              vl_activ6r,
                         USING v_msid 'PPPI_ACTIVITY_1_UNIT'         vl_activ1_un,
                         USING v_msid 'PPPI_ACTIVITY_2_UNIT'         vl_activ2_un,
                         USING v_msid 'PPPI_ACTIVITY_3_UNIT'         vl_activ3_un,
                         USING v_msid 'PPPI_ACTIVITY_4_UNIT'         vl_activ4_un,
                         USING v_msid 'PPPI_ACTIVITY_5_UNIT'         vl_activ5_un,
                         USING v_msid 'PPPI_ACTIVITY_6_UNIT'         vl_activ6_un,
                         USING v_msid 'PPPI_POSTING_DATE'            wa_notif-fecha_inicio.


ENDFORM.                    "hoja_salario


*&---------------------------------------------------------------------*
*&      Form  consumos
*&---------------------------------------------------------------------*
*     Linea N => Consumo de Mercancías
*     Consumo de Mercancías
*     PI_CONS
*----------------------------------------------------------------------*
FORM consumos.
  DATA: vl_operation LIKE wa_atributos-valor,
        vl_phase     LIKE wa_atributos-valor,
        vl_peso_real LIKE wa_atributos-valor,
        vl_unit      LIKE wa_atributos-valor,
        vl_batch     LIKE wa_atributos-valor,
        vl_material  LIKE wa_atributos-valor,
        vl_mat_quant LIKE wa_atributos-valor,
        vl_mtart     TYPE mara-mtart,
        vl_rsnum     TYPE afko-rsnum,
        vl_bdmng     TYPE resb-bdmng,
        vl_werks     TYPE werks_d,
        vl_bstfe     TYPE marc-bstfe,
        vl_indice    TYPE i.

  CLEAR: it_procmessheader[],
         it_procmesscharac[],
         it_procmessheaderreturn[],
         it_procmesscharacreturn[].

  CLEAR wa_buscar.
  CLEAR wa_procmessheader.
  PERFORM get_sender_name USING c_cm CHANGING wa_procmessheader-sender_name.
  wa_procmessheader-proc_mess_id_tmp   = v_msid.
  PERFORM get_centro USING wa_notif-num_op CHANGING wa_procmessheader-plant.
  wa_procmessheader-proc_mess_category = c_cm.
  wa_procmessheader-test_flag          = ''.
  APPEND wa_procmessheader TO it_procmessheader.

  CLEAR: vl_operation,
         vl_phase,
         vl_peso_real,
         vl_unit,
         vl_material,
         vl_mat_quant.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_OPERATION'.
  IF sy-subrc EQ 0.
    vl_operation = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_PHASE'.
  IF sy-subrc EQ 0.
    vl_phase = wa_atributos-valor.
  ENDIF.

*  CLEAR wa_atributos.
*  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
*                                                     atributo = 'PPPI_UNIT_OF_MEASURE'.
*  IF sy-subrc EQ 0.
*    vl_unit = wa_atributos-valor.
*  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_BATCH'.
  IF sy-subrc EQ 0.
    "vl_batch = wa_atributos-valor.
    vl_batch = wa_notif-lote.
  ENDIF.

  CLEAR vl_indice.
  PERFORM formatear_material USING wa_notif-cod_componente CHANGING vl_material.
  LOOP AT it_atributos INTO wa_atributos WHERE clase EQ 'Z0000005'
                                           AND atributo EQ 'PPPI_MATERIAL'
                                           AND valor EQ vl_material.
    vl_indice = sy-tabix.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DO 3 TIMES.
      ADD 1 TO vl_indice.
      CLEAR wa_atributos.
      READ TABLE it_atributos INTO wa_atributos INDEX vl_indice.
    ENDDO.
    IF wa_atributos-clase EQ 'Z0000005' AND wa_atributos-atributo EQ 'PPPI_UNIT_OF_MEASURE'.
      vl_unit = wa_atributos-valor.
    ENDIF.
  ENDIF.

  CLEAR vl_mtart.
  SELECT SINGLE mtart
    FROM mara
    INTO vl_mtart
    WHERE matnr EQ vl_material
      AND NOT mtart IN ('ZMPV', 'ZPSE', 'ZPTT', 'ZPTV').
  IF sy-subrc EQ 0.
    CLEAR vl_rsnum.
    SELECT SINGLE rsnum
      FROM afko
      INTO vl_rsnum
      WHERE aufnr EQ v_numop.

    CLEAR vl_bdmng.
    SELECT SINGLE bdmng
      FROM resb
      INTO vl_bdmng
      WHERE rsnum EQ vl_rsnum
        AND matnr EQ vl_material.

    CLEAR vl_werks.
    PERFORM get_centro USING v_numop CHANGING vl_werks.

    CLEAR vl_bstfe.
    SELECT SINGLE bstfe
      FROM marc
      INTO vl_bstfe
      WHERE werks EQ vl_werks
        AND matnr EQ vl_material.

    CLEAR: wa_atributos, vl_mat_quant.
    READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                       atributo = 'PPPI_MATERIAL_QUANTITY'.
    IF sy-subrc EQ 0.
      vl_mat_quant = wa_atributos-valor.
    ENDIF.

    IF vl_mat_quant NE 0.
      vl_peso_real = vl_bdmng * vl_bstfe / vl_mat_quant.
    ELSE.
      vl_peso_real = 0.
    ENDIF.
  ELSE.
    vl_peso_real = wa_notif-peso_real.
  ENDIF.
  REPLACE ALL OCCURRENCES OF '.' IN vl_peso_real WITH ','.

  PERFORM cargar_caract: USING v_msid 'PPPI_PROCESS_ORDER'     v_numop,
                         USING v_msid 'PPPI_EVENT_DATE'        wa_notif-fecha_inicio,
                         USING v_msid 'PPPI_EVENT_TIME'        wa_notif-hora_inicio,
                         USING v_msid 'PPPI_OPERATION'         vl_operation,
                         USING v_msid 'PPPI_PHASE'             vl_phase,
                         USING v_msid 'PPPI_MATERIAL'          vl_material,
                         USING v_msid 'PPPI_MATERIAL_CONSUMED' vl_peso_real,
                         USING v_msid 'PPPI_UNIT_OF_MEASURE'   vl_unit,
                         USING v_msid 'PPPI_BATCH'             vl_batch,
                         USING v_msid 'PPPI_STORAGE_LOCATION'  'MP01',
                         USING v_msid 'PPPI_SPECIAL_STOCK'     ''.
ENDFORM.                    "consumos


*&---------------------------------------------------------------------*
*&      Form  FORMATEAR_MATERIAL
*&---------------------------------------------------------------------*
FORM formatear_material USING u_matnr CHANGING c_matnr.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = u_matnr
    IMPORTING
      output       = c_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "FORMATEAR_MATERIAL


*&---------------------------------------------------------------------*
*&      Form  consumos_no_notificados
*&---------------------------------------------------------------------*
FORM consumos_no_notificados USING u_notif TYPE zpptt_nucleo_notif
                                   u_retorno TYPE zpptt_nucleo_ret.
  DATA: vl_material   LIKE wa_atributos-valor,
        vl_mat_cab    LIKE wa_atributos-valor,
        vl_componente LIKE wa_atributos-valor,
        vl_existe     TYPE c,
        wa_atri_local LIKE wa_atributos,
        vl_orden      LIKE v_numop.

  LOOP AT it_atributos INTO wa_atri_local.
    IF wa_atri_local-clase EQ 'Z0000004' AND wa_atri_local-atributo EQ 'PPPI_PROCESS_ORDER'.
      v_numop = wa_atri_local-valor.
      PERFORM formatea_orden CHANGING v_numop.
    ELSEIF wa_atri_local-clase EQ 'Z0000004' AND wa_atri_local-atributo EQ 'PPPI_MATERIAL'.
      PERFORM formatear_material USING wa_atri_local-valor CHANGING vl_mat_cab.
    ELSEIF wa_atri_local-clase EQ 'Z0000005' AND wa_atri_local-atributo EQ 'PPPI_MATERIAL'.
      CLEAR vl_material.
      PERFORM formatear_material USING wa_atri_local-valor CHANGING vl_material.

      CLEAR vl_existe.
      LOOP AT u_notif INTO wa_notif.
        PERFORM formatear_material USING wa_notif-cod_componente CHANGING vl_componente.
        IF vl_componente EQ vl_material.
          vl_existe = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF vl_existe IS INITIAL.
        v_msid = v_msid + 1.
        CLEAR wa_notif.
        LOOP AT u_notif INTO wa_notif.
          vl_orden = wa_notif-num_op.
          PERFORM formatea_orden CHANGING vl_orden.
          IF vl_orden EQ v_numop.
            EXIT.
          ENDIF.
        ENDLOOP.
        PERFORM notif_comp_no_notif USING v_numop vl_material u_retorno vl_mat_cab.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "consumos_no_notificados


*&---------------------------------------------------------------------*
*&      Form  notif_comp_no_notif
*&---------------------------------------------------------------------*
FORM notif_comp_no_notif USING u_numop
                               u_material
                               u_retorno TYPE zpptt_nucleo_ret
                               u_mat_cab.

  TYPES: BEGIN OF ty_lqua,
           charg TYPE lqua-charg,
           verme TYPE lqua-verme,
           bdatu TYPE lqua-bdatu,
         END OF ty_lqua.

  DATA: it_lqua TYPE TABLE OF ty_lqua,
        wa_lqua TYPE ty_lqua.

  DATA: vl_operation LIKE wa_atributos-valor,
        vl_phase     LIKE wa_atributos-valor,
        vl_peso_real LIKE wa_atributos-valor,
        vl_unit      LIKE wa_atributos-valor,
        vl_batch     LIKE wa_atributos-valor,
        vl_material  LIKE wa_atributos-valor,
        vl_mat_quant LIKE wa_atributos-valor,
        vl_mtart     TYPE mara-mtart,
        vl_rsnum     TYPE afko-rsnum,
        vl_bdmng     TYPE resb-bdmng,
        vl_werks     TYPE werks_d,
        vl_bstfe     TYPE marc-bstfe,
        vl_indice    TYPE i,
        vl_peso_rest LIKE wa_atributos-valor, " peso restante
        vl_peso_cons LIKE wa_atributos-valor. " peso consumir

  CLEAR: it_procmessheader[],
         it_procmesscharac[],
         it_procmessheaderreturn[],
         it_procmesscharacreturn[].

  CLEAR wa_buscar.
  CLEAR wa_procmessheader.
  PERFORM get_sender_name USING c_cm CHANGING wa_procmessheader-sender_name.
  wa_procmessheader-proc_mess_id_tmp   = v_msid.
  PERFORM get_centro USING u_numop CHANGING wa_procmessheader-plant.
  wa_procmessheader-proc_mess_category = c_cm.
  wa_procmessheader-test_flag          = ''.
  APPEND wa_procmessheader TO it_procmessheader.

  CLEAR: vl_operation,
         vl_phase,
         vl_peso_real,
         vl_unit,
         vl_material,
         vl_mat_quant.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_OPERATION'.
  IF sy-subrc EQ 0.
    vl_operation = wa_atributos-valor.
  ENDIF.

  CLEAR wa_atributos.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000005'
                                                     atributo = 'PPPI_PHASE'.
  IF sy-subrc EQ 0.
    vl_phase = wa_atributos-valor.
  ENDIF.

  CLEAR vl_werks.
  PERFORM get_centro USING v_numop CHANGING vl_werks.

  LOOP AT it_atributos INTO wa_atributos WHERE clase EQ 'Z0000005'
                                           AND atributo EQ 'PPPI_MATERIAL'
                                           AND valor EQ u_material.
    vl_indice = sy-tabix.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DO 3 TIMES.
      ADD 1 TO vl_indice.
      CLEAR wa_atributos.
      READ TABLE it_atributos INTO wa_atributos INDEX vl_indice.
    ENDDO.
    IF wa_atributos-clase EQ 'Z0000005' AND wa_atributos-atributo EQ 'PPPI_UNIT_OF_MEASURE'.
      vl_unit = wa_atributos-valor.
    ENDIF.
  ENDIF.

  CLEAR vl_rsnum.
  SELECT SINGLE rsnum
    FROM afko
    INTO vl_rsnum
    WHERE aufnr EQ v_numop.

  CLEAR vl_bdmng.
  SELECT SINGLE bdmng
    FROM resb
    INTO vl_bdmng
    WHERE rsnum EQ vl_rsnum
      AND matnr EQ u_material.

  CLEAR vl_bstfe.
  SELECT SINGLE bstfe
    FROM marc
    INTO vl_bstfe
    WHERE werks EQ vl_werks
      AND matnr EQ u_mat_cab.

  CLEAR: wa_atributos, vl_mat_quant.
  READ TABLE it_atributos INTO wa_atributos WITH KEY clase    = 'Z0000004'
                                                     atributo = 'PPPI_MATERIAL_QUANTITY'.
  IF sy-subrc EQ 0.
    vl_mat_quant = wa_atributos-valor.
  ENDIF.

  IF vl_mat_quant NE 0.
    vl_peso_real = vl_bdmng * vl_bstfe / vl_mat_quant.
    vl_peso_rest = vl_peso_real.
  ELSE.
    vl_peso_real = 0.
  ENDIF.

  REPLACE ALL OCCURRENCES OF '.' IN vl_peso_real WITH ','.

  SELECT charg verme bdatu
    FROM lqua
    INTO TABLE it_lqua
    WHERE lgnum EQ 'WM1'
      AND matnr EQ u_material
      AND werks EQ vl_werks
      AND lgpla EQ 'NUCLEOS'
      AND verme GT 0
    ORDER BY bdatu.

  LOOP AT it_lqua INTO wa_lqua.
    CLEAR vl_peso_cons.
    IF vl_peso_rest GT wa_lqua-verme.
      vl_peso_cons = wa_lqua-verme.
      vl_peso_rest = vl_peso_rest - wa_lqua-verme.
    ELSE.
      vl_peso_cons = vl_peso_rest.
      vl_peso_rest = 0.
    ENDIF.

    IF vl_peso_cons NE 0.
      PERFORM cargar_caract: USING v_msid 'PPPI_PROCESS_ORDER'     v_numop,
                             USING v_msid 'PPPI_EVENT_DATE'        wa_notif-fecha_inicio,
                             USING v_msid 'PPPI_EVENT_TIME'        wa_notif-hora_inicio,
                             USING v_msid 'PPPI_OPERATION'         vl_operation,
                             USING v_msid 'PPPI_PHASE'             vl_phase,
                             USING v_msid 'PPPI_MATERIAL'          u_material,
                             USING v_msid 'PPPI_MATERIAL_CONSUMED' vl_peso_cons,
                             USING v_msid 'PPPI_UNIT_OF_MEASURE'   vl_unit,
                             USING v_msid 'PPPI_BATCH'             wa_lqua-charg,
                             USING v_msid 'PPPI_STORAGE_LOCATION'  'MP01',
                             USING v_msid 'PPPI_SPECIAL_STOCK'     ''.

      PERFORM ejecutar_bapi TABLES u_retorno.
      v_msid = v_msid + 1.
    ELSE.
      EXIT. "& No hay más consumos a notificar
    ENDIF.
  ENDLOOP.

  IF vl_peso_cons NE 0 AND NOT it_lqua[] IS INITIAL. " Ya se consumieron los lotes
    " No hay stock
    PERFORM cargar_caract: USING v_msid 'PPPI_PROCESS_ORDER'     v_numop,
                           USING v_msid 'PPPI_EVENT_DATE'        wa_notif-fecha_inicio,
                           USING v_msid 'PPPI_EVENT_TIME'        wa_notif-hora_inicio,
                           USING v_msid 'PPPI_OPERATION'         vl_operation,
                           USING v_msid 'PPPI_PHASE'             vl_phase,
                           USING v_msid 'PPPI_MATERIAL'          u_material,
                           USING v_msid 'PPPI_MATERIAL_CONSUMED' vl_peso_cons,
                           USING v_msid 'PPPI_UNIT_OF_MEASURE'   vl_unit,
                           USING v_msid 'PPPI_BATCH'             wa_lqua-charg,
                           USING v_msid 'PPPI_STORAGE_LOCATION'  'MP01',
                           USING v_msid 'PPPI_SPECIAL_STOCK'     ''.

    PERFORM ejecutar_bapi TABLES u_retorno.
    v_msid = v_msid + 1.
  ENDIF.
ENDFORM.                    "notif_comp_no_notif
