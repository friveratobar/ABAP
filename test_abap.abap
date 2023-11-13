*&-------------------------------------------------------------------*
*& Report  ACC_BAPI_SXDA_DOCUMENT_POST                               *
*&                                                                   *
*&-------------------------------------------------------------------*
*& Test report for SXDA Data Workbench                               *
*&-------------------------------------------------------------------*
*--------------------------------------------------------------------*
* PROGRAMA    : ZSD614 ( Basado en ZSD610 )                          *
* FER Nï¿½      :                                                      *
* CHARM Nï¿½    : S 9000008807                                         *
* TIPO        : Interfaz                                             *
* AUTOR       : Felipe Rivera                                        *
* SOLICITANTE : ***** ******                                      *
* FECHA       : 25/02/2019                                           *
* OBJETIVO    : Contabilizaciï¿½n Ventas Intercompany                  *
*--------------------------------------------------------------------*
*                   M O D I F I C A C I O N E S                      *
*--------------------------------------------------------------------*
REPORT zsd614.

DATA tvbdpr TYPE TABLE OF vbdpr WITH HEADER LINE.

DATA: BEGIN OF tkomv OCCURS 50.
    INCLUDE STRUCTURE komv.
DATA: END OF tkomv.

DATA: BEGIN OF tkomvd OCCURS 50.
    INCLUDE STRUCTURE komvd.
DATA: END OF tkomvd.

PARAMETERS: receiver LIKE tbdlst-logsys NO-DISPLAY.

*--------------------------------------------------------------------*
* Definition of tables                                               *
*--------------------------------------------------------------------*
TABLES: nast, "Messages
        *nast.                        "Messages

TABLES: komk,
        komp,
        vbdkr,
        tvko.

DATA: anzal LIKE nast-anzal. "HUNGARY
DATA: nast_anzal LIKE nast-anzal. "Number of outputs (Orig. + Cop.)

TABLES: vbco3 ,
*        vbdkr   ,
        vbdpr   ,
        vbdre   ,
        conf_out,
        sadr    ,
*        tvko    ,
        adrs    ,
        t005    ,
        t001    ,
        t001g   ,
        tvcint  ,
        konh    ,
        tlic    ,
        fpltvb  ,
        sdaccdpc.

DATA: BEGIN OF itg_header OCCURS 0,
        comp_code  TYPE t001-bukrs,
        ref_doc_no TYPE bkpf-xblnr,
        doc_date   TYPE bkpf-bldat,
        pstng_date TYPE bkpf-budat,
        doc_type   TYPE bkpf-blart,
        header_txt TYPE bkpf-bktxt,
      END OF itg_header.

*--------------------------------------------------------------------*
* Constantes                                                         *
*--------------------------------------------------------------------*
CONSTANTS: c_zzr5(004) TYPE c VALUE 'ZZR5',
           c_bkpff(05) TYPE c VALUE 'BKPFF',
           c_x(001)    TYPE c VALUE 'X',
           c_vv01      LIKE thead-tdid     VALUE 'VV01',
           c_vbbp      LIKE thead-tdobject VALUE 'VBBP',
           c_id        LIKE thead-tdid     VALUE '0001',
           c_vbbk      LIKE thead-tdobject VALUE 'VBBK'.

DATA:
  gd_documentheader LIKE bapiache09,
  it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE.
*--------------------------------------------------------------------*
* Tablas Internas                                                    *
*--------------------------------------------------------------------*
DATA:
        itg_accountgl      TYPE STANDARD TABLE OF bapiacgl09
      , itg_accounttax     TYPE STANDARD TABLE OF bapiactx09
      , itg_currencyamount TYPE STANDARD TABLE OF bapiaccr09
      , itg_return         TYPE STANDARD TABLE OF bapiret2
      , itg_accountpayable TYPE STANDARD TABLE OF bapiacap09.
*--------------------------------------------------------------------*
* Work area                                                          *
*--------------------------------------------------------------------*
DATA:
       stg_accountgl      TYPE bapiacgl09
     , stg_accounttax     TYPE bapiactx09
     , stg_currencyamount TYPE bapiaccr09
     , stg_return         TYPE bapiret2
     , stg_accountpayable TYPE bapiacap09.

DATA: BEGIN OF stl_obj
        , obj_type  LIKE  bapiache09-obj_type
        , obj_key   LIKE  bapiache09-obj_key
        , obj_sys   LIKE  bapiache09-obj_sys
      , END OF stl_obj.

*--------------------------------------------------------------------*
* Variables Globales                                                 *
*--------------------------------------------------------------------*
DATA gv_tax_code TYPE mwskz.
DATA gv_lines TYPE i.
DATA vg_error TYPE c LENGTH 001.
DATA i_error TYPE c LENGTH 001.
DATA gv_kunnr TYPE tvko-kunnr.
DATA gv_factor TYPE char2 VALUE 1.
*--------------------------------------------------------------------*
* Specific data of ENTRY_CH                                          *
*--------------------------------------------------------------------*
DATA print_local_curr_ch.
DATA komvdk_ch LIKE komvd OCCURS 10 WITH HEADER LINE.
DATA komvdp_ch LIKE komvd OCCURS 10 WITH HEADER LINE.
*--------------------------------------------------------------------*
* ENTRY                                                              *
*--------------------------------------------------------------------*
FORM entry USING return_code us_screen.
  PERFORM authority_check.
  PERFORM fill_internal_tables.
  PERFORM f_ale_acc_doc.
  IF vg_error IS INITIAL.
    return_code = 0.
  ELSE.
    return_code = 3.
  ENDIF.
ENDFORM. "ENTRY
*&---------------------------------------------------------------------*
*&      Form  F_ALE_ACC_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_ale_acc_doc.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = gd_documentheader
    IMPORTING
      obj_type       = stl_obj-obj_type
      obj_key        = stl_obj-obj_key
      obj_sys        = stl_obj-obj_sys
    TABLES
      accountpayable = itg_accountpayable
      accountgl      = itg_accountgl
      currencyamount = itg_currencyamount
      accounttax     = itg_accounttax
      return         = it_return.

  "code Inspector Ok.
  CLEAR vg_error.
  LOOP AT it_return WHERE type EQ 'E'.
    vg_error = c_x.
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb              = it_return-id
        msg_nr                 = it_return-number
        msg_ty                 = it_return-type
        msg_v1                 = it_return-message_v1
        msg_v2                 = it_return-message_v2
        msg_v3                 = it_return-message_v3
        msg_v4                 = it_return-message_v4
      EXCEPTIONS
        message_type_not_valid = 1
        no_sy_message          = 2
        OTHERS                 = 3.

  ENDLOOP.

  IF vg_error IS INITIAL.
    CLEAR it_return.
    it_return-id          = '69'.
    it_return-number      = '260'.
    it_return-type        = 'S'.
    it_return-message_v1  = stl_obj-obj_key.

    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb              = it_return-id
        msg_nr                 = it_return-number
        msg_ty                 = it_return-type
        msg_v1                 = it_return-message_v1
        msg_v2                 = it_return-message_v2
        msg_v3                 = it_return-message_v3
        msg_v4                 = it_return-message_v4
      EXCEPTIONS
        message_type_not_valid = 1
        no_sy_message          = 2
        OTHERS                 = 3.

    IF sy-batch IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDIF.


*  PERFORM show_messages.

  REFRESH: itg_accountgl,
           itg_currencyamount,
           it_return,
           itg_accountpayable,
           itg_accounttax.

  CLEAR: gd_documentheader.

ENDFORM. "F_ALE_ACC_DOC

*---------------------------------------------------------------------*
*      Form  fill_internal_tables
*---------------------------------------------------------------------*
FORM fill_internal_tables.
* PERFORM get_data IN PROGRAM rvadin01.
  "Search Data / Info documento ventas
  PERFORM fill_data.                " Rutina de Busqueda de data, y preparacion.
  PERFORM document_header.          " Cabecera BAPI
  PERFORM fill_accountpayable.      " Linea de proveedor
  PERFORM fill_currencyamount_prov. " Currency linea de proveedor
  PERFORM fill_accountgl.           " Contabilizacion (Cta  Mayor)

ENDFORM. " fill_internal_tables
*---------------------------------------------------------------------*
*      Form  Show_messages
*---------------------------------------------------------------------*
FORM show_messages.
*  IF it_return[] IS INITIAL.
*    WRITE: / 'no messages'.
*  ELSE.
*    SKIP 1.
*    LOOP AT it_return.
*      WRITE: /    it_return-type,
*             (2)  it_return-id,
*                  it_return-number,
*             (80) it_return-message,
**                 IT_RETURN-LOG_NO
**                 IT_RETURN-LOG_MSG_NO
**                 IT_RETURN-MESSAGE_V1
**                 IT_RETURN-MESSAGE_V2
**                 IT_RETURN-MESSAGE_V3
**                 IT_RETURN-MESSAGE_V4
*             (20) it_return-parameter,
*             (3)  it_return-row,
*                  it_return-field.
**                 IT_RETURN-SYSTEM
*    ENDLOOP.
*  ENDIF.
*
*  ULINE.
ENDFORM. " Show_messages
*---------------------------------------------------------------------*
*       FORM fill_accountgl                                           *
*---------------------------------------------------------------------*
FORM fill_accountgl.

  DATA: vl_skont TYPE t076i-skont,
        vl_ccost TYPE bapi0012_gen-costcenter,
        vl_carea TYPE bapi0012_gen-co_area,
        stg_cost TYPE bapi0012_ccoutputlist,
        vl_kokrs LIKE tka02-kokrs,
        vl_bukrs LIKE t001-bukrs,
        vl_name  LIKE thead-tdname,
        vl_xblnr TYPE bkpf-xblnr,
        vl_e     TYPE char1 VALUE 'E'.

  DATA itg_tline TYPE STANDARD TABLE OF tline.
  DATA stg_tline TYPE tline.
  DATA aux_accountgl TYPE bapiacgl09.
  REFRESH it_return. CLEAR it_return.

  LOOP AT tvbdpr.
    CLEAR: vl_ccost, vl_name, vl_kokrs, vl_carea,stg_cost.

    CLEAR gv_lines.
    DESCRIBE TABLE itg_accountgl LINES gv_lines.


    IF gv_lines > 0.
      CLEAR aux_accountgl.
      READ TABLE itg_accountgl INTO aux_accountgl INDEX gv_lines.
      IF aux_accountgl IS NOT INITIAL.
        stg_accountgl-itemno_acc  = aux_accountgl-itemno_acc + 1. " Nï¿½mero de posiciï¿½n del documento FI
      ENDIF.
    ELSE.
      stg_accountgl-itemno_acc   = '2'.
    ENDIF.

    PERFORM cuenta_mayor USING vl_skont tvbdpr-matnr.
    stg_accountgl-gl_account  = vl_skont .    " Cuenta de mayor de la contabilidad principal
    stg_accountgl-acct_type   = 'S'.          " Clase de cuenta

    " Clase de documento
    CASE vbdkr-vbtyp.
      WHEN 'M'.
        stg_accountgl-doc_type = 'KR'.
      WHEN 'O'.
        stg_accountgl-doc_type = 'KC'.
      WHEN 'P'.
        stg_accountgl-doc_type = 'KD'.
      WHEN OTHERS.
        syst-msgty = 'E'.
        PERFORM protocol_update IN PROGRAM rvadin01.
    ENDCASE.

    CONCATENATE vbdkr-vbeln tvbdpr-posnr INTO vl_name. "Numero Factura + Posicion.

* Centro de costo
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = c_vv01
        language                = sy-langu
        name                    = vl_name
        object                  = c_vbbp
      TABLES
        lines                   = itg_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 0.
      CLEAR stg_tline.
      READ TABLE itg_tline INTO stg_tline INDEX 1.
      IF stg_tline IS NOT INITIAL.
        vl_ccost  = stg_tline-tdline.
        CONDENSE vl_ccost.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vl_ccost
          IMPORTING
            output = vl_ccost.
      ENDIF.
    ENDIF.

* Variables
    CALL FUNCTION 'KOKRS_GET_FROM_BUKRS'
      EXPORTING
        i_bukrs        = vbdkr-bukrs " Sociedad
      IMPORTING
        e_kokrs        = vl_kokrs
      EXCEPTIONS
        no_kokrs_found = 1
        OTHERS         = 2.
    IF  sy-subrc EQ 0.
      vl_carea = vl_kokrs.
    ENDIF.

    CALL FUNCTION 'BAPI_COSTCENTER_GETDETAIL1'
      EXPORTING
        controllingarea      = vl_carea " Controlling Area
        costcenter           = vl_ccost " Centro Costo
        keydate              = sy-datum " Fecha Hoy
        master_data_inactive = ' '      " n/a
*       LANGUAGE             =          " n/a
      IMPORTING
        costcenterdetail     = stg_cost
      TABLES
        return               = itg_return.
    IF sy-subrc EQ 0.
      stg_accountgl-bus_area = stg_cost-bus_area.   " ï¿½rea funcional
    ENDIF.

    stg_accountgl-fis_period  = vbdkr-fkdat+04(02). " Mes contable
    stg_accountgl-fisc_year   = vbdkr-fkdat+00(04). " Ejercicio
    stg_accountgl-pstng_date  = vbdkr-fkdat.        " Fecha de contabilizaciï¿½n en el documento
    stg_accountgl-value_date  = vbdkr-fkdat.        " Fecha valor
    stg_accountgl-tax_code    = gv_tax_code.        " Indicador IVA
    stg_accountgl-costcenter  = vl_ccost.           " Centro de coste

    PERFORM fill_currencyamount_pos USING stg_accountgl-itemno_acc.  " Importes Posiciones

    IF tvbdpr-mwsbp IS NOT INITIAL. " Si hay impuesto por posicion
      PERFORM fill_accounttax_pos USING stg_accountgl-itemno_acc
                                        vl_skont.
    ENDIF.

    APPEND stg_accountgl TO itg_accountgl.
    CLEAR stg_accountgl.

  ENDLOOP.

ENDFORM. "fill_accountgl
*---------------------------------------------------------------------*
*       FORM fill_header                                              *
*---------------------------------------------------------------------*
FORM document_header.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = gd_documentheader-obj_sys.          " Sistema lï¿½gico documento de origen
  gd_documentheader-obj_type   = c_bkpff.                      " Operaciï¿½n de referencia
  gd_documentheader-username   = sy-uname.                     " Nombre del usuario
  gd_documentheader-header_txt = itg_header-header_txt.        " Texto de cabecera
  gd_documentheader-comp_code  = itg_header-comp_code.         " Sociedad
  gd_documentheader-doc_date   = itg_header-doc_date.          " Fecha de documento en documento
  gd_documentheader-pstng_date = itg_header-pstng_date.        " Fecha de contabilizaciï¿½n en el documento
  gd_documentheader-fisc_year  = itg_header-pstng_date+00(04). " Ejercicio
  gd_documentheader-doc_type   = itg_header-doc_type.          " Clase de documento
  gd_documentheader-ref_doc_no = itg_header-ref_doc_no.        " Nï¿½mero de documento de referencia

ENDFORM. "fill_header
*---------------------------------------------------------------------*
*       FORM fill_currencyamount                                      *
*---------------------------------------------------------------------*
FORM fill_currencyamount_pos USING p_itemno_acc TYPE posnr_acc.
* USING p_tabix TYPE sy-tabix.

* Define factor +/-
  CASE vbdkr-vbtyp.
    WHEN 'M'. " (KR) Factura
      gv_factor = 1.
    WHEN 'O'. " (KC) Nota de Credito
      gv_factor = -1.
    WHEN 'P'. " (KD) Nota de Debito
      gv_factor = 1.
    WHEN OTHERS.
      syst-msgty = 'E'.
      PERFORM protocol_update IN PROGRAM rvadin01.
  ENDCASE.

*  DATA aux_currencyamount TYPE bapiaccr09.
*
*  CLEAR gv_lines.
*  DESCRIBE TABLE itg_currencyamount LINES gv_lines.
*
*
*
*  IF gv_lines > 0.
*
*    CLEAR aux_currencyamount.
*    READ TABLE itg_currencyamount INTO aux_currencyamount INDEX p_tabix.
*      IF stg_currencyamount IS NOT INITIAL.
*          stg_currencyamount-itemno_acc = aux_currencyamount-itemno_acc + 1.
*      ENDIF.
*  ELSE.
*     stg_currencyamount-itemno_acc   = '2'.
*  ENDIF.

  stg_currencyamount-itemno_acc  = p_itemno_acc.
  stg_currencyamount-currency    = tvbdpr-waerk. " Moneda

  " Importe en moneda de documento
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = tvbdpr-waerk " Moneda
      amount_internal = tvbdpr-netwr " Monto
    IMPORTING
      amount_external = stg_currencyamount-amt_doccur.
  IF sy-subrc EQ 0.
    stg_currencyamount-amt_doccur  =  stg_currencyamount-amt_doccur * gv_factor.
  ENDIF.

  " Importe en moneda de documento
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = tvbdpr-waerk " Moneda
      amount_internal = tvbdpr-netwr " Monto
    IMPORTING
      amount_external = stg_currencyamount-amt_base.
  IF sy-subrc EQ 0.
    stg_currencyamount-amt_base    =  stg_currencyamount-amt_base * gv_factor.
  ENDIF.

  IF tvbdpr-mwsbp IS NOT INITIAL.
    " Importe base del impuesto en moneda documento
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = tvbdpr-waerk  " Moneda
        amount_internal = tvbdpr-mwsbp  " Impuesto
      IMPORTING
        amount_external = stg_currencyamount-tax_amt.
    IF sy-subrc EQ 0.
      stg_currencyamount-tax_amt = stg_currencyamount-tax_amt * gv_factor.
    ENDIF.
  ENDIF.

  " Importe base del impuesto en moneda documento
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = tvbdpr-waerk  " Moneda
*      amount_internal = tvbdpr-mwsbp " Impuesto
*    IMPORTING
*      amount_external = stg_currencyamount-amt_base.
*  IF sy-subrc EQ 0.
*    stg_currencyamount-amt_base = stg_currencyamount-amt_base * gv_factor.
*  ENDIF.

  APPEND stg_currencyamount TO itg_currencyamount.
  CLEAR stg_currencyamount.

ENDFORM. "fill_currencyamount
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data .

  DATA: vl_name  LIKE thead-tdname,
        vl_xblnr TYPE bkpf-xblnr,
        vl_e     TYPE char1 VALUE 'E'.

  DATA itg_tline TYPE STANDARD TABLE OF tline.
  DATA stg_tline TYPE tline.

  CLEAR: komk, komp, nast_anzal, sdaccdpc,vbco3,vbdkr.
  REFRESH tvbdpr.

* Chequeamos que sea el mensaje para intercompany nuevo.
  CHECK nast-kschl = c_zzr5.

  CALL FUNCTION 'RV_PRICE_PRINT_REFRESH'
    TABLES
      tkomv = tkomv.

  IF nast-objky+10(6) NE space.
    vbco3-vbeln = nast-objky+16(10).
  ELSE.
    vbco3-vbeln = nast-objky.
  ENDIF.

  vbco3-mandt = sy-mandt.
  vbco3-spras = nast-spras.
  vbco3-kunde = nast-parnr.
  vbco3-parvw = nast-parvw.

  CALL FUNCTION 'RV_BILLING_PRINT_VIEW'
    EXPORTING
      comwa                        = vbco3
    IMPORTING
      kopf                         = vbdkr "kopf
    TABLES
      pos                          = tvbdpr
    EXCEPTIONS
      terms_of_payment_not_in_t052 = 1
      error_message                = 5
      OTHERS                       = 4.

  IF NOT sy-subrc IS INITIAL.
    IF sy-subrc = 1.
      syst-msgty = 'I'.
      PERFORM protocol_update IN PROGRAM.
    ENDIF.
  ENDIF.

  CLEAR komk.
  komk-mandt = sy-mandt.
  komk-kalsm = vbdkr-kalsm.
  komk-fkart = vbdkr-fkart.
  komk-kappl = 'V'.

  IF vbdkr-kappl NE space.
    komk-kappl = vbdkr-kappl.
  ENDIF.

  komk-waerk = vbdkr-waerk.
  komk-knumv = vbdkr-knumv.
  komk-vbtyp = vbdkr-vbtyp.
  komk-bukrs = vbdkr-bukrs.
  komk-belnr = vbdkr-vbeln.
  komp-kposn = vbdpr-posnr.

  CALL FUNCTION 'RV_PRICE_PRINT_ITEM'
    EXPORTING
      comm_head_i = komk
      comm_item_i = komp
    IMPORTING
      comm_head_e = komk
      comm_item_e = komp
    TABLES
      tkomv       = tkomv
      tkomvd      = tkomvd.

  vl_name = nast-objky. "Numero Factura

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = c_id
      language                = sy-langu
      name                    = vl_name
      object                  = c_vbbk
    TABLES
      lines                   = itg_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.
    CLEAR stg_tline.
    READ TABLE itg_tline INTO stg_tline INDEX 1.
    IF stg_tline IS NOT INITIAL.
      itg_header-header_txt = stg_tline-tdline.
    ENDIF.
  ENDIF.

  itg_header-comp_code  = vbdkr-vbund.
  itg_header-doc_date   = vbdkr-erdat.
  itg_header-pstng_date = vbdkr-fkdat.

  CASE vbdkr-vbtyp.
    WHEN 'M'.
      itg_header-doc_type = 'KR'.
    WHEN 'O'.
      itg_header-doc_type = 'KC'.
    WHEN 'P'.
      itg_header-doc_type = 'KD'.
    WHEN OTHERS.
      syst-msgty = 'E'.
      PERFORM protocol_update IN PROGRAM rvadin01.
  ENDCASE.

  CONCATENATE vl_e vbdkr-xblnr INTO vl_xblnr.
  itg_header-ref_doc_no = vl_xblnr.

  CLEAR gv_tax_code.


ENDFORM. " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check .
  AUTHORITY-CHECK OBJECT 'ZBC_PROGRA'
      ID 'PROGRAM' FIELD sy-repid.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e014(zi) WITH TEXT-t02. "'***ï¿½ï¿½ï¿½Sin autorizaciï¿½n para correr este programa!!!***'.
  ENDIF.
ENDFORM. " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  FILL_ACCOUNTPAYABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_accountpayable.

  DATA: vl_name       LIKE thead-tdname,
        vl_xblnr      TYPE bkpf-xblnr,
        vl_e          TYPE char1 VALUE 'E',
        vl_bukrs      TYPE tvko-bukrs,
        vl_zterm      TYPE lfb1-zterm,
        vl_zwels      TYPE lfb1-zwels,
        vl_alloc_nmbr TYPE acpi_zuonr,
        vl_item_text  TYPE sgtxt,
        vl_buzei      TYPE bseg-buzei,
        vl_gjahr      TYPE bseg-gjahr,
        vl_mwskz      TYPE bset-mwskz.

  DATA itg_tline TYPE STANDARD TABLE OF tline.
  DATA stg_tline TYPE tline.

  SELECT kunnr bukrs INTO ( gv_kunnr, vl_bukrs )
  FROM tvko UP TO 1 ROWS
  WHERE vkorg EQ vbdkr-vkorg
  ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF gv_kunnr IS NOT INITIAL.
    PERFORM tax_code.                 " tax_code
  ENDIF.

  SELECT zterm zwels INTO ( vl_zterm, vl_zwels )
  FROM lfb1 UP TO 1 ROWS
  WHERE lifnr EQ gv_kunnr AND
        bukrs EQ vl_bukrs
  ORDER BY PRIMARY KEY.
  ENDSELECT.

  CONCATENATE 'E' vbdkr-xblnr INTO vl_alloc_nmbr.

  vl_name = vbdkr-vbeln.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = c_id
      language                = sy-langu
      name                    = vl_name
      object                  = c_vbbk
    TABLES
      lines                   = itg_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.
    CLEAR stg_tline.
    READ TABLE itg_tline INTO stg_tline INDEX 1.
    IF stg_tline IS NOT INITIAL.
      vl_item_text = stg_tline-tdline.
    ENDIF.
  ENDIF.

  stg_accountpayable-itemno_acc = '1'.            " Nï¿½mero de posiciï¿½n del documento FI
  stg_accountpayable-vendor_no  = gv_kunnr.       " Nï¿½mero de cuenta del proveedor o acreedor
  stg_accountpayable-pmnttrms   = vl_zterm.       " Clave de condiciones de pago
  stg_accountpayable-pymt_meth  = vl_zwels.       " Vï¿½a de pago
*  stg_accountpayable-pmnt_block = 'P'.            " Clave para bloqueo de pago
  stg_accountpayable-alloc_nmbr = vl_alloc_nmbr.  " Nï¿½mero de asignaciï¿½n
  stg_accountpayable-item_text  = vl_item_text.   " Texto posiciï¿½n
  stg_accountpayable-tax_code   = gv_tax_code.    " Indicador IVA

  APPEND stg_accountpayable TO itg_accountpayable.
  CLEAR stg_accountpayable.

  IF vbdkr-mwsbk IS NOT INITIAL.
    PERFORM fill_accounttax_prov.          " Impuestos Proveedor
  ENDIF.
ENDFORM." fill_accountpayable
*&---------------------------------------------------------------------*
*&      Form  TAX_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tax_code.

  DATA:
    vl_bukrs    TYPE tvko-bukrs,
    vl_buzei    TYPE bseg-buzei,
    vl_gjahr    TYPE bseg-gjahr,
    vl_mwskz    TYPE bset-mwskz,
    vl_mwsatz   TYPE t076m-mwsatz,
    vl_kbetr    TYPE bset-kbetr,
    vl_impuesto TYPE p  DECIMALS 3.

  "tax_code
  SELECT buzei gjahr INTO ( vl_buzei, vl_gjahr )
  FROM bseg UP TO 1 ROWS
  WHERE bukrs EQ vbdkr-bukrs AND
        belnr EQ vbdkr-vbeln AND
        gjahr EQ vbdkr-fkdat+0(4) AND
        koart EQ 'D'
  ORDER BY PRIMARY KEY.
  ENDSELECT.

  SELECT mwskz kbetr INTO ( vl_mwskz, vl_kbetr )
  FROM bset UP TO 1 ROWS
  WHERE bukrs EQ vbdkr-bukrs AND
        belnr EQ vbdkr-vbeln AND
        gjahr EQ vl_gjahr    AND
        buzei EQ vl_buzei
  ORDER BY PRIMARY KEY.
  ENDSELECT.

  IF vl_kbetr IS NOT INITIAL.
    READ TABLE tkomv WITH KEY kbetr = vl_kbetr.
    IF  sy-subrc EQ 0.
      IF tkomv-krech = 'A' AND
         tkomv-waers = ''.           " Valor Porcentual !
        IF vl_kbetr > 0.
          vl_impuesto  = vl_kbetr / 10. " 190,00 --> 19.000
          vl_mwsatz = vl_impuesto.
          CONDENSE vl_mwsatz.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    vl_mwsatz = '0.000'.
  ENDIF.

  SELECT mwskz INTO vl_mwskz
  FROM t076m UP TO 1 ROWS
  WHERE parart = 'LI'        AND
        konto  = gv_kunnr    AND
        mwart  = vl_mwskz    AND
        land1  = vbdkr-landa AND
        mwsatz = vl_mwsatz
  ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc EQ 0.
    gv_tax_code = vl_mwskz.
  ELSE.
    gv_tax_code = 'C0'.     " sin impuestos
  ENDIF.

ENDFORM. " tax_code
*&---------------------------------------------------------------------*
*&      Form  FILL_CURRENCYAMOUNT_PROV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_currencyamount_prov.
  DATA vl_netwr TYPE netwr.

* Define factor +/-
  CASE vbdkr-vbtyp.
    WHEN 'M'. " (KR) Factura
      gv_factor = -1.
    WHEN 'O'. " (KC) Nota de Credito
      gv_factor =  1.
    WHEN 'P'. " (KD) Nota de Debito
      gv_factor = -1.
    WHEN OTHERS.
      syst-msgty = 'E'.
      PERFORM protocol_update IN PROGRAM rvadin01.
  ENDCASE.

* De la linea del proveedor Accountoayable (-) , accountgl(+)
  stg_currencyamount-itemno_acc = '1'.         " Nï¿½mero de posiciï¿½n del documento FI
  stg_currencyamount-currency   = vbdkr-waerk. " Moneda
*vl_netwr = vbdkr-netwr + vbdkr-mwsbk.        " Importe

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = vbdkr-waerk " Moneda
      amount_internal = vbdkr-netwr " Monto neto
    IMPORTING
      amount_external = stg_currencyamount-amt_doccur.
  IF sy-subrc EQ 0.
    stg_currencyamount-amt_doccur = stg_currencyamount-amt_doccur * gv_factor.
  ENDIF.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = vbdkr-waerk " Moneda
      amount_internal = vbdkr-netwr " Monto neto
    IMPORTING
      amount_external = stg_currencyamount-amt_base.
  IF sy-subrc EQ 0.
    stg_currencyamount-amt_base = stg_currencyamount-amt_base * gv_factor.
  ENDIF.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = vbdkr-waerk " Moneda
      amount_internal = vbdkr-mwsbk " Impuesto
    IMPORTING
      amount_external = stg_currencyamount-tax_amt.
  IF sy-subrc EQ 0.
    stg_currencyamount-tax_amt = stg_currencyamount-tax_amt * gv_factor.
  ENDIF.

  APPEND stg_currencyamount TO itg_currencyamount.
  CLEAR stg_currencyamount.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Numero_cuenta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cuenta_mayor USING p_skont TYPE t076i-skont
                        p_matnr TYPE matnr.

  SELECT skont INTO p_skont
  FROM t076i UP TO 1 ROWS
  WHERE parart  = 'LI'        AND
        konto   = gv_kunnr    AND
        ktbukrs = vbdkr-vbund AND
        ewlnr   = p_matnr
  ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_ACCOUNTTAX_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_accounttax_prov.
*  stg_accounttax-itemno_acc = '1'.
*  stg_accounttax-tax_code   = 'C1'.
*  APPEND stg_accounttax TO itg_accounttax.
*  CLEAR stg_accounttax.
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  FILL_CURRENCYAMOUNT_TAX
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM fill_currencyamount_tax_prov.
*  DATA vl_itemno_acc    TYPE posnr_acc.
*  DATA stg_aux_currency TYPE bapiaccr09.
*
*  CLEAR gv_lines.
*  DESCRIBE TABLE itg_currencyamount LINES gv_lines.
*
*  CLEAR stg_aux_currency.
*  READ TABLE itg_currencyamount INTO stg_aux_currency INDEX gv_lines.
*  IF stg_aux_currency IS NOT INITIAL.
*    vl_itemno_acc = stg_aux_currency-itemno_acc + 1.
*  ENDIF.
*
*  stg_currencyamount-itemno_acc = vl_itemno_acc. " Nï¿½mero de posiciï¿½n del documento FI
*  stg_currencyamount-currency   = vbdkr-waerk.   " Clave de moneda
*
*  " Importe en moneda de documento
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = vbdkr-waerk " Moneda
*      amount_internal = vbdkr-mwsbk " Monto
*    IMPORTING
*      amount_external = stg_currencyamount-amt_doccur.
*
*  " Importe base del impuesto en moneda documento
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = vbdkr-waerk " Moneda
*      amount_internal = vbdkr-netwr " Monto
*    IMPORTING
*      amount_external = stg_currencyamount-amt_base.
*
*  APPEND stg_currencyamount TO itg_currencyamount.
*  CLEAR stg_currencyamount.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_ACCOUNTTAX_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_accounttax_pos USING p_itemno_acc TYPE posnr_acc
                               p_skont      TYPE t076i-skont.
*  DATA aux_accounttax TYPE bapiactx09.
*  CLEAR gv_lines.
*  DESCRIBE TABLE itg_accounttax LINES gv_lines.
*  IF gv_lines > 0.
*    CLEAR aux_accounttax.
*    READ TABLE itg_accounttax INTO aux_accounttax INDEX gv_lines.
*    IF aux_accounttax IS NOT INITIAL.
*      stg_accounttax-itemno_acc = aux_accounttax-itemno_acc + 1.
*    ENDIF.
*  ELSE.
*    stg_accounttax-itemno_acc = '2'.
*  ENDIF.

*  stg_accounttax-itemno_acc = p_itemno_acc.
*  stg_accounttax-tax_code   = 'C1'.
*  stg_accounttax-gl_account = p_skont.
*  APPEND stg_accounttax TO itg_accounttax.
*  CLEAR stg_accounttax.

*tb_accounttax-itemno_acc = 3.
*tb_accounttax-gl_account = '0000121710'.
*tb_accounttax-tax_code = 'P1'.
*tb_accounttax-TAX_RATE = '20'.
ENDFORM.