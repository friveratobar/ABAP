
*Code listing for: *******

*Description: Report agreement rebate

REPORT zsd463n_ar NO STANDARD PAGE HEADING MESSAGE-ID zi.
************************************************************************
* PROGRAMA    : ***************************                            *
* FER N°      : ********                                               *
* TIPO        : Reporte                                                *
* AUTOR       : **********                                             *
* SOLICITANTE : **********                                             *
* FECHA       : ***********                                            *
* OBJETIVO    : Reporte que permite visualizar el monto total de provi-*
*               siones generadas y el monto total de provisiones       *
*               pendientes por cada N° de acuerdo.(Copia ZSD463_AR)    *
************************************************************************
*                   M O D I F I C A C I O N E S                        *
************************************************************************
* 10/08/2018  Prog: **************      Sol.: ************             *
*             Ajustes parámetros de entrada Fecha de inicio y fin N°   *
*             de pedido del cliente. Ajuste a la columna tipo de doc,  *
*             en la columna Tipo de Doc descripción no contabilizado.  *
*             ***********************************                      *
************************************************************************

TABLES: kona, "Acuerdos
        konp, "Condiciones (Posición)
        vbrk, "Factura: Datos de cabecera
        vbrp,
        makt,
        t6b2f,
        vbox,
        konv,
        konh,
        s060. "Estructura modelo de estr. info con VAKEY delante

CONSTANTS:
  cg_k       TYPE fktyp  VALUE 'K',
  cg_s       TYPE vbtyp  VALUE 'S',
  cg_x       TYPE fksto  VALUE 'X',
  cg_b       TYPE krech  VALUE 'B',
  cg_abono   TYPE char5  VALUE 'ABONO',
  cg_factura TYPE char7  VALUE 'FACTURA',
  cg_abonor  TYPE char11 VALUE 'ABONO RAPEL',
  cg_otro    TYPE char4  VALUE 'OTRO',
  cg_sin_doc TYPE char11 VALUE 'Pos.Sin.Doc',
  cg_vbtyp_o TYPE vbtyp  VALUE 'O',
  cg_vbtyp_m TYPE vbtyp  VALUE 'M',
  cg_zarh    TYPE kschl  VALUE 'ZARH',
  cg_zarg    TYPE kschl  VALUE 'ZARG',
  cg_zarf    TYPE kschl  VALUE 'ZARF',
  cg_za51    TYPE fkart  VALUE 'ZA51',
  cg_za67    TYPE fkart  VALUE 'ZA67'.


TYPES: BEGIN OF tyg_kona,
         knuma TYPE kona-knuma,
         bonem TYPE kona-bonem,
         boart TYPE kona-boart,
         datab TYPE kona-datab,
         datbi TYPE kona-datbi,
         vkorg TYPE kona-vkorg,
         vtweg TYPE kona-vtweg,
         spart TYPE kona-spart,
         bosta TYPE kona-bosta,
         erdat TYPE kona-erdat,
       END OF tyg_kona,

       BEGIN OF tyg_vbrk,
         fktyp TYPE vbrk-fktyp,
         vbtyp TYPE vbrk-vbtyp,
         vkorg TYPE vbrk-vkorg,
         vtweg TYPE vbrk-vtweg,
         spart TYPE vbrk-spart,
         kunag TYPE vbrk-kunag,
         knumv TYPE vbrk-knumv,
         fksto TYPE vbrk-fksto,
       END OF tyg_vbrk,

       BEGIN OF tyg_vrbp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         matnr TYPE vbrp-matnr,
       END OF tyg_vrbp,

       BEGIN OF tyg_makt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
         maktg TYPE makt-maktg,
       END OF tyg_makt,

       BEGIN OF tyg_konv,
         knumv TYPE konv-knumv,
         kschl TYPE konv-kschl,
         knumh TYPE konv-knumh,
         kwert TYPE konv-kwert,
       END OF tyg_konv,

       BEGIN OF tyg_konp,
         knumh TYPE konp-knumh,
         krech TYPE konp-krech,
       END OF tyg_konp,

       BEGIN OF tyg_s060,
         knumh   TYPE s060-knumh,
         ruwrt_k TYPE s060-ruwrt_k,
         kawrt_k TYPE s060-kawrt_k,
         kstbs   TYPE s060-kstbs,
         auwrt_k TYPE s060-auwrt_k,
         rrwrt_k TYPE s060-rrwrt_k,
         ruwrt   TYPE s060-ruwrt,
         kwaeh   TYPE s060-kwaeh,
         kzbzg   TYPE s060-kzbzg,
       END OF tyg_s060.

TYPES:
  BEGIN OF tyg_tsum,
    knuma   LIKE kona-knuma,     "Núm.Acuerdo Rapel
    kschl   LIKE t6b2f-kschl ,   "Clase de Condición
    knumh   LIKE kote700-knumh,  "Nº Registro Cond.
    kwert_k LIKE konv-kwert_k,   " Total Peso
    ruwrt_m LIKE s060-ruwrt_k,   "Provisión Manual
    auwrt_k LIKE s060-auwrt_k,   "Pago rappel
    rrwrt_k LIKE s060-rrwrt_k,   "Atrasos liquidados
    volum   LIKE s060-rrwrt_k,   "Volumen de negocios
  END OF tyg_tsum,

  BEGIN OF tyg_detalle_paso,
    knuma LIKE kona-knuma,    "Núm.Acuerdo Rapel
    kschl LIKE t6b2f-kschl ,  "Clase de Condición
    knumh LIKE kote700-knumh, "Nº Registro Cond.
  END OF tyg_detalle_paso,

  BEGIN OF tyg_konv3,
    knumv TYPE konv-knumv,
    kposn TYPE konv-kposn,
    kschl TYPE konv-kschl,
    knumh TYPE konv-knumh,
    kwert TYPE konv-kwert,
    kawrt TYPE konv-kawrt,
    kstbs TYPE konv-kstbs,
  END OF tyg_konv3.

************************************************************************
*                 T A B L A S   I N T E R N A S                        *
************************************************************************
DATA: BEGIN OF itg_salida OCCURS 0,
        knuma    LIKE kona-knuma,     "Acuerdo
        bonem    LIKE kona-bonem,     "Beneficiario
        boart    LIKE kona-boart,     "Cl. de acuerdo
        datab    LIKE kona-datab,     "Válido de
        datbi    LIKE kona-datbi,     "Fin de validez
        budat    LIKE bkpf-budat,     "Fecha Contable
        vbeln    LIKE vbrp-vbeln,     "Documentos comerciales
        tipo_doc TYPE char13,         "Des. Tipo de documento
        posnr    LIKE vbrp-posnr,     "Posición
        matnr    LIKE vbrp-matnr,     "Material
        maktx    LIKE makt-maktx,     "Descripción del material
        pedido   LIKE vbfa-vbelv,     "Pedido Comercial
        kawrt_k  LIKE s060-kawrt_k,   "Base de condición
        kstbs    LIKE s060-kstbs,     "Base de escala
*        ruwrt_k   LIKE s060-ruwrt_k,   "
        ruwrt_m  LIKE s060-ruwrt_k,   "Provisión Manual
        auwrt_k  LIKE s060-auwrt_k,   "Pago rappel
        rrwrt_k  LIKE s060-rrwrt_k,   "Atrasos liquidados
        volum    LIKE s060-rrwrt_k,   "Volumen de negocios
        kwaeh1   LIKE s060-kwaeh,     "Moneda
        kschl    LIKE t6b2f-kschl,    "Clase de Condición
        kwert_k  LIKE konv-kwert_k,   "Provisión o Total pesos
        knumh    LIKE konv-knumh,     "Nº registro condición
        color    TYPE char4,
        bold     TYPE lvc_t_styl,
        bstkd    LIKE vbkd-bstkd,
      END OF itg_salida.
*
DATA: BEGIN OF itg_kona OCCURS 0,
        knuma   LIKE kona-knuma , "Nº del Acuerdo.
        bonem   LIKE kona-bonem , "Código Beneficiario.
        datbi   LIKE kona-datbi , "Fin Validez.
        vkorg   LIKE kona-vkorg , "Org. Ventas
        vtweg   LIKE kona-vtweg , "Canal Distribución
        spart   LIKE kona-spart , "Sector
        bosta   LIKE kona-bosta , "Status de Acuerdo.
        erdat   LIKE kona-erdat , "Creado el
        boart   LIKE kona-boart , "Clase del Acuerdo.
        knumh   LIKE konh-knumh , "Nº de Registro Condición.
        vtext   LIKE t6b1t-vtext, "Descripción Clase del Acuerdo.
        name1   LIKE kna1-name1 , "Nonbre del Beneficiario.
        datab   LIKE kona-datab , "Inicio Validez.
        aedat   LIKE kona-aedat , "Fecha última modificación
        kobog   LIKE kona-kobog , "Grupo de Condición.
        kschl   LIKE t6b2f-kschl, " Clase de Condición
        vakey   LIKE konh-vakey,  " Clave Var.
        kotabnr LIKE konh-kotabnr, " Tabla.
        kappl   LIKE konh-kappl,  " Aplicación.
        waers   LIKE kona-waers,
        check   TYPE c,
      END OF itg_kona.
*
DATA: BEGIN OF itg_vbrk OCCURS 0,
        vkorg LIKE vbrk-vkorg , "Org. Ventas
        vtweg LIKE vbrk-vtweg , "Canal Distribución
        spart LIKE vbrk-spart , "Sector
        kunag LIKE vbrk-kunag , "Status de Acuerdo.
        knumv LIKE vbrk-knumv , "N° condición documento
      END OF itg_vbrk.
*
DATA: BEGIN OF itg_konv OCCURS 0,
        knumv   LIKE konv-knumv ,  "N° condición documento
        kschl   LIKE konv-kschl ,  "Clase condición
        knumh   LIKE konv-knumh ,  "N° registro de condición
        kwert_k LIKE konv-kwert_k, "Valor de la condición
      END OF itg_konv.
*
*DATA: BEGIN OF itg_konp OCCURS 0,
*        knumh    LIKE konp-knumh ,   "N° registro de condición
*        knuma_bo LIKE konp-knuma_bo, "Acuerdo
*        kunnr    LIKE konp-kunnr,    "Solicitante
*      END OF itg_konp.
*
DATA: BEGIN OF itg_s060 OCCURS 0,
        knumh   LIKE s060-knumh ,   "N° registro de condición
        ruwrt_k LIKE s060-ruwrt_k , "Provisión
        auwrt_k LIKE s060-auwrt_k , "Pago Rappel
        rrwrt_k LIKE s060-rrwrt_k , "Atrasos liquidados
        kwaeh   LIKE s060-kwaeh ,   "Moneda
        kawrt_k LIKE s060-kawrt_k,  "Base de condición
        kstbs   LIKE s060-kstbs,    "Base de escala
      END OF itg_s060.
*
DATA: BEGIN OF i_detalle OCCURS 0 ,
        knuma    LIKE kona-knuma,    "Núm.Acuerdo Rapel
        bonem    LIKE kona-bonem ,   "Código Beneficiario.
        name1    LIKE kna1-name1,    "Nombre cliente
        vtext    LIKE t6b1t-vtext,   "Descripción Clase del Acuerdo.
        datab    LIKE kona-datab ,   "Inicio Validez.
        datbi    LIKE kona-datbi ,   "Fin Validez.
        boart    LIKE kona-boart,    "Cl.acuerdo
        waers    TYPE kona-waers,    "Moneda
        kwert    LIKE konv-kwert ,   "Total
        kwert_k  LIKE konv-kwert_k,  "Total pesos
        kschl    LIKE t6b2f-kschl ,  " Clase de Condición
        knumh    LIKE kote700-knumh, " Nº Registro Cond.
        knumv    LIKE vbrk-knumv,
        fkart    LIKE vbrk-fkart,    "Condición
        vakey    LIKE konh-vakey,    " Clave Var.
*        bonus   LIKE kote702-bonus,
        kotabnr  LIKE konh-kotabnr,  " Tabla.
        kappl    LIKE konh-kappl,    " Aplicación.
        vbeln    LIKE vbox-vbeln,
        posnr    LIKE vbox-posnr,
        budat    LIKE bkpf-budat,
        matnr    LIKE vbrp-matnr,
        maktx    LIKE makt-maktx,
        vbelv    LIKE vbfa-vbelv,
        vbtyp    LIKE vbrk-vbtyp,
        kawrt_k  LIKE s060-kawrt_k,
        kstbs    LIKE s060-kstbs,
        rrwrt_k  LIKE s060-rrwrt_k,
        ruwrt_m  LIKE s060-ruwrt_k, "Provisión Manual
        lparcial TYPE char1,
        pmanual  TYPE char1,
      END OF i_detalle.

TYPES:
  BEGIN OF tyg_vbrk_abonos,
    vbeln TYPE vbrk-vbeln,
    knuma TYPE vbrk-knuma,
    vbtyp TYPE vbrk-vbtyp,
    knumv TYPE vbrk-knumv,
  END OF tyg_vbrk_abonos,

  BEGIN OF tyg_vbrp_abonos,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    netwr TYPE vbrp-netwr,
    matnr TYPE vbrp-matnr,
  END OF tyg_vbrp_abonos,

  BEGIN OF tyg_acuerdos,
    knuma TYPE vbrk-knuma,
  END OF tyg_acuerdos.

DATA: BEGIN OF i_t6b2f OCCURS 0.
    INCLUDE STRUCTURE i_detalle.
DATA: END OF i_t6b2f.

DATA: BEGIN OF i_kotex OCCURS 0 .
    INCLUDE STRUCTURE i_t6b2f.
DATA: END OF i_kotex.
*
DATA: BEGIN OF t_anul OCCURS 0,
        sfakn LIKE vbrk-sfakn, " DOCUMENTO ANULACION
      END OF t_anul.
*
DATA xdatbi LIKE s060-spmon.
*
RANGES x_bosta FOR kona-bosta.
*
DATA: no_entry(2) VALUE '. '.
************************************************************************
*                   TABLAS INTERNAS                                    *
************************************************************************
DATA:
  itg_vbox        TYPE STANDARD TABLE OF vbox,         stg_vbox      TYPE vbox,
  itg_vbrp        TYPE STANDARD TABLE OF vbrp,         stg_vbrp      TYPE vbrp,
  itg_matnr       TYPE STANDARD TABLE OF matnr,        stg_matnr     TYPE matnr,
  itg_mktx        TYPE STANDARD TABLE OF maktx,        stg_mktx      TYPE maktx,
  itg_makt        TYPE STANDARD TABLE OF makt,         stg_makt      TYPE makt,
  itg_bkpf        TYPE STANDARD TABLE OF bkpf,         stg_bkpf      TYPE bkpf,
  itg_konv3       TYPE STANDARD TABLE OF tyg_konv3,    stg_konv3     TYPE tyg_konv3,
  itg_vbfa        TYPE STANDARD TABLE OF vbfa,         stg_vbfa      TYPE vbfa,
  itg_vbrp_abonos TYPE STANDARD TABLE OF tyg_vbrp_abonos, stg_vbrp_abonos TYPE tyg_vbrp_abonos,
  itg_vbrk_abonos TYPE STANDARD TABLE OF tyg_vbrk_abonos, stg_vbrk_abonos TYPE tyg_vbrk_abonos,
  itg_tsum        TYPE STANDARD TABLE OF tyg_tsum,               stg_tsum TYPE tyg_tsum,
  itg_paso        TYPE STANDARD TABLE OF tyg_detalle_paso,       stg_paso TYPE tyg_detalle_paso,
  itg_acuerdos    TYPE STANDARD TABLE OF tyg_acuerdos,       stg_acuerdos TYPE tyg_acuerdos,
  itg_konp        TYPE STANDARD TABLE OF tyg_konp,               stg_konp TYPE tyg_konp.

DATA: itg_kona2 TYPE TABLE OF tyg_kona,
      itg_vbrk2 TYPE TABLE OF tyg_vbrk,
      itg_konv2 TYPE TABLE OF tyg_konv.
*      itg_konp2 TYPE TABLE OF tyg_konp.
************************************************************************
*                     V A R I A B L E S   D E L   A L V                *
************************************************************************
TYPE-POOLS: slis.
*
INCLUDE <icon>.
*
DATA: vg_sort TYPE slis_t_sortinfo_alv,
      vg_print             TYPE slis_print_alv,
      vg_layout            TYPE lvc_s_layo,
      vg_events            TYPE slis_t_event,
      vg_variant           TYPE disvariant,
      vg_keyinfo           TYPE slis_keyinfo_alv,
      vg_fieldcat          TYPE lvc_t_fcat,
      vg_list_top_of_page  TYPE slis_t_listheader,
      vg_slis_sp_group_alv TYPE slis_t_sp_group_alv.

DATA: lw_bold TYPE lvc_s_styl,
      lt_bold TYPE lvc_t_styl.
*
DATA: vg_display_variant LIKE disvariant,
      vg_default_fieldcat TYPE slis_t_fieldcat_alv.
*
DATA: vg_repid LIKE sy-repid.
*
DATA vg_linea TYPE i.
*
*
************************************************************************
*                 PANTALLA DE SELECCION                                *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-t01.
*
SELECT-OPTIONS: s_knuma FOR kona-knuma OBLIGATORY, "Acuerdo
                s_bonem  FOR kona-bonem.                "Beneficiario de rappel

PARAMETERS: p_datba LIKE kona-datab, " Requerimiento SD-3428 RAR
            p_datbi LIKE kona-datbi, "OBLIGATORY.    "Fin de validez del acuerdo --- Se quita obligatoriedad --- 20/05/2016
            p_boart LIKE kona-boart.  " Requerimiento SD-3428 RAR

SELECT-OPTIONS: s_ident3 FOR kona-ident3, "Período de liquidación para acuerdos
                s_vkorg  FOR kona-vkorg,                "Org. de Ventas
                s_vtweg  FOR kona-vtweg,                "Canal distribución
                s_spart  FOR kona-spart.                "Sector
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-t02.
*
PARAMETERS: xpend DEFAULT 'X' AS CHECKBOX, "Pendientes
            xverif   DEFAULT 'X' AS CHECKBOX,    "Se verifica liberación
            xabono   AS CHECKBOX,                "Sol.abono p.rappel creada
            xliberac DEFAULT 'X' AS CHECKBOX,    "Liberación correcta
            xliq     AS CHECKBOX.                "Liquidación final
*
SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN END OF BLOCK bl1.
*
AT SELECTION-SCREEN ON s_vkorg.
*
*  PERFORM AUTHORITY_CHECK_VKORG TABLES S_VKORG.
************************************************************************
*  PROGRAMA PRINCIPAL                                                  *
************************************************************************
START-OF-SELECTION.
**--- Rango de status de liquidación rappel
  PERFORM init_bosta.
*
  AUTHORITY-CHECK OBJECT 'ZBC_PROGRA'
     ID 'PROGRAM' FIELD sy-repid.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e014(zi) WITH TEXT-t02. "'***¡¡¡Sin autorización para correr este programa!!!***'.
  ENDIF.
*
* Busqueda de Datos.
  PERFORM rescata_informacion.
*
*  DESCRIBE TABLE itg_salida LINES vg_linea.
  "IF vg_linea < 0.
  PERFORM f_provision_manual.
*
  PERFORM despliega_lista.
*ELSE.
*MESSAGE i014(zi) WITH TEXT-t03.
*ENDIF.

***---
************************************************************************
*    RUTINA PARA VALIDAR AUTORIZACION PARA PROCESAR ORG. DE VENTAS     *
************************************************************************
FORM authority_check_vkorg TABLES xvkorg.

* internal fields
  DATA: BEGIN OF ti_orgvta OCCURS 0,
          vkorg LIKE tvko-vkorg,
        END OF ti_orgvta.

** Rescata las organizaciones de venta a validar
  SELECT vkorg INTO TABLE ti_orgvta FROM tvko
         WHERE vkorg IN xvkorg.

** Ciclo para validar los centros a procesar
  LOOP AT ti_orgvta.
    AUTHORITY-CHECK OBJECT 'V_VBRK_VKO'
              ID 'VKORG' FIELD ti_orgvta-vkorg
              ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      MESSAGE e215(zi) WITH ti_orgvta-vkorg.
      EXIT.
    ENDIF.
  ENDLOOP.
*
ENDFORM. "AUTHORITY_CHECK_VKORG
*&---------------------------------------------------------------------*
*&      Form  RESCATA_INFORMACION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rescata_informacion .
  CLEAR itg_kona.
  REFRESH itg_kona.

  CONSTANTS: lc_i     TYPE c VALUE 'I',
             lc_eq(2) TYPE c VALUE 'EQ',
             lc_bt(2) TYPE c VALUE 'BT'.

  DATA: rl_boart TYPE RANGE OF kona-boart,
        sl_boart LIKE LINE OF rl_boart.

  " Requerimiento SD-3428 RAR
  IF NOT p_boart IS INITIAL.
    sl_boart-sign   = lc_i.
    sl_boart-option = lc_eq.
    sl_boart-low    = p_boart.
    APPEND sl_boart TO rl_boart.
  ENDIF.
  " Requerimiento SD-3428 RAR


**--Busqueda de acuerdos a la fecha.
  IF NOT p_datba IS INITIAL AND NOT p_datbi IS INITIAL. " Requerimiento SD-3428 RAR


    SELECT knuma bonem datbi vkorg vtweg spart bosta erdat boart
           datab aedat kobog waers FROM kona
             INTO CORRESPONDING FIELDS OF TABLE itg_kona
                WHERE knuma  IN s_knuma
                 AND  boart  IN rl_boart " Requerimiento SD-3428 RAR
                 AND  bonem  IN s_bonem
                 AND  bosta  IN x_bosta
                 AND  datab  EQ p_datba " Requerimiento SD-3428 RAR
                 AND  datbi  EQ p_datbi
                 AND  ident3 IN s_ident3
                 AND  vkorg  IN s_vkorg
                 AND  vtweg  IN s_vtweg
                 AND  spart  IN s_spart.

  ELSEIF p_datba IS INITIAL AND p_datbi IS INITIAL. " Requerimiento SD-3428 RAR

    SELECT knuma bonem datbi vkorg vtweg spart bosta erdat boart
     datab aedat kobog waers FROM kona
       INTO CORRESPONDING FIELDS OF TABLE itg_kona
          WHERE knuma  IN s_knuma
           AND  boart  IN rl_boart " Requerimiento SD-3428 RAR
           AND  bonem  IN s_bonem
           AND  bosta  IN x_bosta
*                 AND  datab  EQ p_datba " Requerimiento SD-3428 RAR
*                   AND  datbi  EQ p_datbi
           AND  ident3 IN s_ident3
           AND  vkorg  IN s_vkorg
           AND  vtweg  IN s_vtweg
           AND  spart  IN s_spart.


  ELSEIF p_datbi IS NOT INITIAL. " Requerimiento SD-3428 RAR
*  IF NOT p_datba IS INITIAL.
    SELECT knuma bonem datbi vkorg vtweg spart bosta erdat boart
           datab aedat kobog waers FROM kona
             INTO CORRESPONDING FIELDS OF TABLE itg_kona
                WHERE knuma  IN s_knuma
                 AND  boart  IN rl_boart " Requerimiento SD-3428 RAR
                 AND  bonem  IN s_bonem
                 AND  bosta  IN x_bosta
*                 AND  datab  EQ p_datba " Requerimiento SD-3428 RAR
                 AND  datbi  EQ p_datbi
                 AND  ident3 IN s_ident3
                 AND  vkorg  IN s_vkorg
                 AND  vtweg  IN s_vtweg
                 AND  spart  IN s_spart.

  ELSEIF p_datba IS NOT INITIAL. " Requerimiento SD-3428 RAR

    SELECT knuma bonem datbi vkorg vtweg spart bosta erdat boart
             datab aedat kobog waers FROM kona
               INTO CORRESPONDING FIELDS OF TABLE itg_kona
                  WHERE knuma  IN s_knuma
                   AND  boart  IN rl_boart " Requerimiento SD-3428 RAR
                   AND  bonem  IN s_bonem
                   AND  bosta  IN x_bosta
                   AND  datab  EQ p_datba " Requerimiento SD-3428 RAR
                   AND  ident3 IN s_ident3
                   AND  vkorg  IN s_vkorg
                   AND  vtweg  IN s_vtweg
                   AND  spart  IN s_spart.
  ENDIF.

  PERFORM f_clase_cond.
**
ENDFORM. " RESCATA_INFORMACION
*----------------------------------------------------------------------*
*      Form  DESPLIEGA_LISTA                                           *
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text                                                 *
*  <--  p2        text                                                 *
*----------------------------------------------------------------------*
FORM despliega_lista .
*
  MOVE sy-repid TO vg_repid.
*--- EVENTOS
  PERFORM eventos.
* -- CATALOGO
  PERFORM fieldcat.
* -- LAYOUT
  PERFORM layout.
* -- TOP-OF-PAGE
  PERFORM comment_build USING vg_list_top_of_page[].
*
  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = vg_display_variant
    EXCEPTIONS
      OTHERS     = 1.
*
  vg_variant = vg_display_variant.
* -- IMPRIME REPORTE
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_buffer_active         = space
      i_callback_program      = vg_repid
*     is_layout               = vg_layout
      is_layout_lvc           = vg_layout
      i_save                  = 'A'
      i_callback_user_command = 'USER_COMMAND'
*     I_CALLBACK_PF_STATUS_SET = 'USER_STATUS'
      it_fieldcat_lvc         = vg_fieldcat[]
      it_events               = vg_events[]
      i_structure_name        = 'TI_SALIDA'
    TABLES
      t_outtab                = itg_salida.
*
ENDFORM. " DESPLIEGA_LISTA
*----------------------------------------------------------------------*
*      Form  EVENTOS                                                   *
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text                                                 *
*  <--  p2        text                                                 *
*----------------------------------------------------------------------*
FORM eventos.
*
  DATA : vl_event  TYPE slis_alv_event.
*
  REFRESH vg_events.
  vl_event-name = 'TOP_OF_PAGE'.
  vl_event-form = 'ALV_TOP_OF_PAGE'.
  APPEND vl_event TO vg_events.
*
ENDFORM. " EVENTOS
*----------------------------------------------------------------------*
*      Form  LAYOUT                                                    *
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text                                                 *
*  <--  p2        text                                                 *
*----------------------------------------------------------------------*
FORM layout.
*
*  vg_layout-zebra             = 'X'.
*  vg_layout-box_fname         = 'RUWRT_M'.
*  vg_layout-cwidth_opt        = ' '.
*  vg_layout-ctab_fname        = 'CELLCOLOR'.
*  vg_layout-stylefname        = 'HANDLE_STYLE'.
*  vg_variant-report = vg_repid.
*  vg_variant-handle = 2.
*
  vg_layout-zebra      = 'X'.
  vg_layout-cwidth_opt = 'X'.
  vg_layout-info_fname = 'COLOR'.
  vg_layout-stylefname = 'BOLD'.
*
ENDFORM. "layout
*----------------------------------------------------------------------*
*      Form  FIELDCAT                                                  *
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text                                        *
*      -->P_0488   text                                                *
*----------------------------------------------------------------------*
FORM fieldcat.
*
  DATA: vl_fieldcat TYPE lvc_s_fcat.
*
  DATA: lv_col TYPE i VALUE 0.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'KNUMA'.
  vl_fieldcat-scrtext_l = TEXT-001.
  vl_fieldcat-hotspot   = 'X'.
  vl_fieldcat-outputlen = '10'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'BONEM'.
  vl_fieldcat-scrtext_l = TEXT-002.
  vl_fieldcat-outputlen = '10'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'BOART'.
  vl_fieldcat-scrtext_l = TEXT-009.
  vl_fieldcat-outputlen = '09'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'DATAB'.
  vl_fieldcat-scrtext_l = TEXT-010.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'DATBI'.
  vl_fieldcat-scrtext_l = TEXT-011.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col            = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'BUDAT'.
  vl_fieldcat-scrtext_l = TEXT-016.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'VBELN'.
  vl_fieldcat-scrtext_l = TEXT-017.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'TIPO_DOC'.
  vl_fieldcat-scrtext_l = TEXT-022.
  vl_fieldcat-outputlen = '07'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'POSNR'.
  vl_fieldcat-scrtext_l = TEXT-021.
  vl_fieldcat-outputlen = '3'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'MATNR'.
  vl_fieldcat-scrtext_l = TEXT-018.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'MAKTX'.
  vl_fieldcat-scrtext_l = TEXT-019.
  vl_fieldcat-outputlen = '30'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'PEDIDO'.
  vl_fieldcat-scrtext_l = TEXT-020.
  vl_fieldcat-outputlen = '13'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'KAWRT_K'.
  vl_fieldcat-scrtext_l = TEXT-012.
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'KSTBS'.
  vl_fieldcat-scrtext_l = TEXT-013.
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.
***--- fin
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
*  vl_fieldcat-fieldname = 'RUWRT_K'.
  vl_fieldcat-fieldname = 'KWERT_K'.
  vl_fieldcat-scrtext_l = TEXT-003.
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'RUWRT_M'.
  vl_fieldcat-scrtext_l = TEXT-015.  "Provisión Manual
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.

  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'AUWRT_K'.
  vl_fieldcat-scrtext_l = TEXT-005.
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.

* Atrasos liquidados
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'RRWRT_K'.
  vl_fieldcat-scrtext_l = TEXT-007.
  vl_fieldcat-outputlen = '15'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'VOLUM'.
  vl_fieldcat-scrtext_l = TEXT-014.
  vl_fieldcat-outputlen = '18'.
  APPEND vl_fieldcat TO vg_fieldcat.
*
  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'KWAEH1'.
  vl_fieldcat-scrtext_l = TEXT-004.
  vl_fieldcat-outputlen = '06'.
  APPEND vl_fieldcat TO vg_fieldcat.

  " Inicio Requerimiento SD-3428 RAR

  CLEAR vl_fieldcat.
  lv_col                = 1 + lv_col.
  vl_fieldcat-col_pos   = lv_col.
  vl_fieldcat-tabname   = 'ITG_SALIDA'.
  vl_fieldcat-fieldname = 'BSTKD'.
  vl_fieldcat-scrtext_l = TEXT-023.
  vl_fieldcat-outputlen = '06'.
  APPEND vl_fieldcat TO vg_fieldcat.

  " Fin Requerimiento SD-3428 RAR

*
ENDFORM. " FIELDCAT
*      -->P_GT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      Form  COMMENT_BUILD                                             *
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*      -->P_GT_LIST_TOP_OF_PAGE[]  text                                *
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
*
  DATA: texto(10),
        ls_line    TYPE slis_listheader.
*
  REFRESH lt_top_of_page.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = TEXT-ti1.
  APPEND ls_line TO lt_top_of_page.
*
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-ti4.
  WRITE sy-uname TO texto.
  ls_line-info = texto.
  APPEND ls_line TO lt_top_of_page.
*
  ls_line-key  = TEXT-ti2.
  WRITE sy-datum TO texto.
  ls_line-info = texto.
  APPEND ls_line TO lt_top_of_page.
*
  ls_line-key  = TEXT-ti3.
  WRITE sy-uzeit TO texto.
  ls_line-info = texto.
  APPEND ls_line TO lt_top_of_page.
*
ENDFORM. " COMMENT_BUILD
*--------------------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                                         *
*--------------------------------------------------------------------------------*
FORM alv_top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'COCACOLA_LOGO'
      it_list_commentary = vg_list_top_of_page.
*
ENDFORM. "alv_top_of_page
*&---------------------------------------------------------------------*
*&      Form  F_CLASE_COND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_clase_cond .

  CLEAR i_t6b2f.
  REFRESH i_t6b2f.

  LOOP AT itg_kona.
    SELECT kotabnr kschl FROM t6b2f INTO (t6b2f-kotabnr,t6b2f-kschl) WHERE kobog EQ itg_kona-boart.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING itg_kona TO i_t6b2f.
        MOVE t6b2f-kotabnr TO i_t6b2f-kotabnr.
        MOVE t6b2f-kschl   TO i_t6b2f-kschl.
        APPEND i_t6b2f.
        CLEAR  i_t6b2f.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

  LOOP AT i_t6b2f.

    CASE i_t6b2f-kotabnr.
********************************************************************
* Condiciones Trivento
      WHEN '593'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote593
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '585'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote585
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '586'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote586
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '587'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote587
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '705'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote705
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '605'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote605
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '685'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote685
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '686'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote686
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
      WHEN '687'.
        SELECT  knumh INTO i_t6b2f-knumh
         FROM kote687
           WHERE knuma EQ i_t6b2f-knuma
            AND  kschl EQ i_t6b2f-kschl.
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING i_t6b2f TO i_kotex.
            APPEND i_kotex.
            CLEAR i_kotex.
          ENDIF.
        ENDSELECT.
*********************************************************************
* Fin Condiciones Trivento
*********************************************************************
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  CLEAR i_t6b2f.
  REFRESH i_t6b2f.

  LOOP AT i_kotex.
    MOVE-CORRESPONDING i_kotex TO i_t6b2f .
    APPEND i_t6b2f.
    CLEAR i_t6b2f.
  ENDLOOP.
**
  LOOP AT i_t6b2f.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_t6b2f-knumh
      IMPORTING
        output = i_t6b2f-knumh.

    SELECT SINGLE vakey kotabnr kappl
       INTO (konh-vakey, konh-kotabnr, konh-kappl)
             FROM konh
             WHERE   knumh EQ i_t6b2f-knumh
              AND knuma_bo EQ i_t6b2f-knuma.

    IF sy-subrc = 0.
      MOVE konh-kotabnr TO i_t6b2f-kotabnr.
      MOVE konh-vakey   TO i_t6b2f-vakey.
      MOVE konh-kappl   TO i_t6b2f-kappl.
      MODIFY i_t6b2f INDEX sy-tabix.

    ENDIF.
  ENDLOOP.
**
  PERFORM f_agrega_cant.
ENDFORM. " F_CLASE_COND
*&---------------------------------------------------------------------*
*&      Form  F_AGREGA_CANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_agrega_cant .
  DATA: vl_tabix TYPE sy-tabix.
  DATA: vl_budat TYPE bkpf-budat.
  DATA: vl_vbelv TYPE vbfa-vbelv.
  DATA: vl_vbeln TYPE vbrk-vbeln,
        vl_knuma TYPE vbrk-knuma.

  CLEAR i_detalle.
  REFRESH i_detalle.
  LOOP AT i_t6b2f.

    SELECT vbeln posnr
     INTO (vbox-vbeln, vbox-posnr)
           FROM vbox
           WHERE kappl   EQ i_t6b2f-kappl
            AND  kotabnr EQ i_t6b2f-kotabnr
            AND  vakey   EQ i_t6b2f-vakey
            AND  ( fbuda >= i_t6b2f-datab
            AND    fbuda <= i_t6b2f-datbi ).

      IF sy-subrc EQ 0.

        MOVE-CORRESPONDING i_t6b2f TO i_detalle.
        MOVE vbox-vbeln TO i_detalle-vbeln.
        MOVE vbox-posnr TO i_detalle-posnr.
        APPEND i_detalle.
        CLEAR i_detalle.

      ENDIF.
    ENDSELECT.

    IF sy-subrc NE 0.

      MOVE-CORRESPONDING i_t6b2f TO i_detalle.
      MOVE '0' TO i_detalle-vbeln.
      MOVE '0' TO i_detalle-posnr.

      APPEND i_detalle.
      CLEAR i_detalle.

    ENDIF.
  ENDLOOP.
*
*** base de condición
  PERFORM f_base_condicion.
*
*** Atrasos liquidados
  PERFORM f_liquidacion_parcial.
*
*** Provision Manual
  PERFORM f_provision_manual_detalle.

  CLEAR i_detalle.
  LOOP AT i_detalle.
    CLEAR vl_tabix.
    vl_tabix = sy-tabix.
*
*** Datos de cabecera del documento
    CLEAR: vbrk-knumv,vbrk-fkart,vbrk-vbtyp.
    SELECT SINGLE knumv fkart vbtyp INTO (vbrk-knumv,vbrk-fkart,vbrk-vbtyp) FROM vbrk
      WHERE vbeln EQ i_detalle-vbeln.
    IF sy-subrc EQ 0 .
      MOVE vbrk-knumv TO i_detalle-knumv.
      MOVE vbrk-fkart TO i_detalle-fkart.
      MOVE vbrk-vbtyp TO i_detalle-vbtyp.
      MODIFY i_detalle INDEX  vl_tabix.
    ENDIF.
*
*** Numero de Material, Base de condición(2)
    CLEAR: vbrp-vbeln, vbrp-posnr, vbrp-matnr, vbrp-netwr.
    SELECT SINGLE vbeln posnr matnr netwr INTO (vbrp-vbeln, vbrp-posnr, vbrp-matnr, vbrp-netwr ) FROM vbrp
       WHERE vbeln EQ i_detalle-vbeln AND
             posnr EQ i_detalle-posnr.
    IF sy-subrc EQ 0.
*      IF i_detalle-kawrt_k IS INITIAL.
*        i_detalle-kawrt_k  = vbrp-netwr.
*      ENDIF.
      MOVE vbrp-matnr TO i_detalle-matnr.
      MODIFY i_detalle INDEX  vl_tabix.
    ENDIF.
*
*** Descripción del material
    CLEAR: makt-maktx.
    SELECT SINGLE maktx INTO ( makt-maktx ) FROM makt
      WHERE matnr EQ i_detalle-matnr AND
            spras EQ sy-langu.
    IF sy-subrc EQ 0.
      MOVE makt-maktx TO i_detalle-maktx.
      MODIFY i_detalle INDEX  vl_tabix.
    ENDIF.
*
*** Fecha Contable
    CLEAR vl_budat.
*{   REPLACE        CTSK902090                                        3
*\    SELECT SINGLE budat INTO ( vl_budat ) FROM bkpf
*\      WHERE belnr EQ i_detalle-vbeln.
    SELECT budat INTO ( vl_budat ) UP TO 1 ROWS
      FROM bkpf
      WHERE belnr EQ i_detalle-vbeln ORDER BY PRIMARY KEY.
    ENDSELECT.
*}   REPLACE
    IF sy-subrc EQ 0.
      MOVE vl_budat TO i_detalle-budat.
      MODIFY i_detalle INDEX  vl_tabix.
    ENDIF.
*
*** Pedido factura
    CLEAR vl_vbelv.
*{   REPLACE        CTSK902090                                        2
*\    SELECT SINGLE vbelv INTO ( vl_vbelv ) FROM vbfa
*\       WHERE vbeln EQ i_detalle-vbeln AND
*\             vbtyp_n EQ 'M' AND
*\             vbtyp_v EQ 'C'.
    SELECT vbelv INTO ( vl_vbelv ) UP TO 1 ROWS
        FROM vbfa
       WHERE vbeln EQ i_detalle-vbeln AND
             vbtyp_n EQ 'M' AND
             vbtyp_v EQ 'C' ORDER BY PRIMARY KEY.
    ENDSELECT.
*}   REPLACE
    IF sy-subrc EQ 0.
      MOVE vl_vbelv TO i_detalle-vbelv.
      MODIFY i_detalle INDEX  vl_tabix.
    ELSE.
*
*** Pedido nota de credito
      CLEAR vl_vbelv.
*{   REPLACE        CTSK902090                                        1
*\      SELECT SINGLE vbelv INTO ( vl_vbelv ) FROM vbfa
*\         WHERE vbeln EQ i_detalle-vbeln AND
*\               vbtyp_n EQ 'P' AND
*\               vbtyp_v EQ 'L'.
      SELECT vbelv INTO ( vl_vbelv ) UP TO 1 ROWS
          FROM vbfa
         WHERE vbeln EQ i_detalle-vbeln AND
               vbtyp_n EQ 'P' AND
               vbtyp_v EQ 'L' ORDER BY PRIMARY KEY.
      ENDSELECT.
*}   REPLACE
      MOVE vl_vbelv TO i_detalle-vbelv.
      MODIFY i_detalle INDEX  vl_tabix.
    ENDIF.
  ENDLOOP.

  PERFORM f_busca_valores.

ENDFORM. " F_AGREGA_CANT
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_valores .
  DATA: w_fksto LIKE vbrk-fksto.
  LOOP AT i_detalle.

    IF i_detalle-vbeln NE 0.

      CLEAR konv-kwert_k.
*{   REPLACE        CTSK902090                                        1
*\      SELECT SINGLE kwert_k INTO konv-kwert_k  FROM konv
*\        WHERE knumv EQ i_detalle-knumv
*\          AND kposn EQ i_detalle-posnr
*\          AND kschl EQ i_detalle-kschl
*\          AND knumh EQ i_detalle-knumh.
      SELECT kwert_k INTO konv-kwert_k  UP TO 1 ROWS
        FROM konv
        WHERE knumv EQ i_detalle-knumv
          AND kposn EQ i_detalle-posnr
          AND kschl EQ i_detalle-kschl
          AND knumh EQ i_detalle-knumh ORDER BY PRIMARY KEY.
      ENDSELECT.
*}   REPLACE

      IF sy-subrc = 0.
        CLEAR: w_fksto.
        SELECT SINGLE fksto FROM vbrk INTO w_fksto
                          WHERE vbeln EQ i_detalle-vbeln.

        IF ( i_detalle-vbtyp EQ 'S' OR i_detalle-vbtyp EQ 'N').
          DELETE i_detalle INDEX sy-tabix.
        ELSE.
          IF ( i_detalle-vbtyp EQ 'P' OR i_detalle-vbtyp EQ 'M').
            SELECT SINGLE sfakn FROM vbrk INTO t_anul-sfakn
                                WHERE vbeln = i_detalle-vbeln.
            IF t_anul-sfakn <> ' ' OR w_fksto = 'X'.
              DELETE i_detalle INDEX sy-tabix.
            ELSE.
              MOVE konv-kwert_k TO i_detalle-kwert_k.
              MODIFY i_detalle INDEX sy-tabix.
            ENDIF.
          ELSE  .
            SELECT SINGLE sfakn FROM vbrk INTO t_anul-sfakn
                                 WHERE vbeln = i_detalle-vbeln.
            IF t_anul-sfakn <> ' ' OR w_fksto = 'X'.
              DELETE i_detalle INDEX sy-tabix.
            ELSE.
              i_detalle-kwert_k = konv-kwert_k * -1.
              MODIFY i_detalle INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE '000000'    TO i_detalle-kwert_k.
      MODIFY i_detalle INDEX sy-tabix .
    ENDIF.
  ENDLOOP.

  SORT i_detalle BY knuma.

  PERFORM f_totaliza.
ENDFORM. " F_BUSCA_VALORES
*&---------------------------------------------------------------------*
*&      Form  F_TOTALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_totaliza .

  CONSTANTS lc_noc TYPE c VALUE 'NO CONTABILIZADO' LENGTH 16.

  DATA lv_bstkd LIKE vbkd-bstkd.

  CLEAR itg_salida.
  REFRESH itg_salida.

  LOOP AT i_detalle.

    CLEAR itg_salida.
    CASE i_detalle-vbtyp.
      WHEN cg_vbtyp_o.

        IF i_detalle-lparcial IS NOT INITIAL.
          itg_salida-tipo_doc = cg_abonor.               "Tipo Documento "Abono Rapel"
        ELSE.
          itg_salida-tipo_doc = cg_abono.                "Tipo Documento "ABONO"
        ENDIF.

        IF i_detalle-kawrt_k > 0.
          itg_salida-kawrt_k = i_detalle-kawrt_k * -1. " Base de condición abono
          itg_salida-kstbs   = i_detalle-kstbs   * -1. " Base escalada
        ELSE.
          itg_salida-kawrt_k = i_detalle-kawrt_k.
          itg_salida-kstbs   = i_detalle-kstbs.
        ENDIF.

      WHEN cg_vbtyp_m.
        itg_salida-tipo_doc = cg_factura.             "Tipo Documento "FACTURA"
        MOVE i_detalle-kawrt_k TO itg_salida-kawrt_k. " Base de condición factura
        MOVE i_detalle-kstbs TO itg_salida-kstbs.     " Base escalada

      WHEN OTHERS.
        IF itg_salida-vbeln IS INITIAL OR
           itg_salida-vbeln EQ 0.
          itg_salida-tipo_doc = cg_sin_doc.
        ELSE.
          itg_salida-tipo_doc = cg_otro.
        ENDIF.
        MOVE i_detalle-kawrt_k TO itg_salida-kawrt_k.
        MOVE i_detalle-kstbs   TO itg_salida-kstbs.
    ENDCASE.

    IF i_detalle-lparcial IS NOT INITIAL OR
           i_detalle-pmanual IS NOT INITIAL.
      itg_salida-kawrt_k = 0.
      itg_salida-kstbs   = 0.
    ENDIF.

    MOVE i_detalle-knuma   TO itg_salida-knuma. "Acuerdo
    MOVE i_detalle-bonem   TO itg_salida-bonem. "Beneficiario
    MOVE i_detalle-boart   TO itg_salida-boart. "Cl. de acuerdo
    MOVE i_detalle-datab   TO itg_salida-datab. "Válido de
    MOVE i_detalle-datbi   TO itg_salida-datbi. "Fin de validez

    IF itg_salida-kwaeh1 IS INITIAL.
      MOVE i_detalle-waers TO itg_salida-kwaeh1. " Moneda
    ENDIF.
    MOVE i_detalle-knumh   TO itg_salida-knumh. "Reg. de Condición
    MOVE i_detalle-kschl   TO itg_salida-kschl. "Clase de Condición
*    MOVE i_detalle-ruwrt_m TO itg_salida-ruwrt_k.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = i_detalle-matnr
      IMPORTING
        output = itg_salida-matnr.

    MOVE i_detalle-vbeln   TO itg_salida-vbeln.  "N° de documento
    MOVE i_detalle-posnr   TO itg_salida-posnr.  "Pos. del documento
    MOVE i_detalle-maktx   TO itg_salida-maktx.  "Des. Material
    MOVE i_detalle-vbelv   TO itg_salida-pedido. "Pedido Comercial
    MOVE i_detalle-budat   TO itg_salida-budat.  "Fecha contable
*
***
    MOVE i_detalle-kwert_k TO itg_salida-kwert_k. "Provisión
    MOVE i_detalle-ruwrt_m TO itg_salida-ruwrt_m. "Provisión Manual
    MOVE i_detalle-rrwrt_k TO itg_salida-rrwrt_k. "Atrasos liquidados
    itg_salida-auwrt_k = i_detalle-rrwrt_k * -1.  "Pago Rapel


    SELECT SINGLE bstkd
      INTO lv_bstkd
      FROM vbkd
      WHERE vbeln EQ i_detalle-vbelv.


    " Inicio Requerimiento SD-3428 RAR

    IF sy-subrc = 0.
      MOVE lv_bstkd TO itg_salida-bstkd.
      CLEAR lv_bstkd.
    ENDIF.

    IF i_detalle-budat IS INITIAL.
      MOVE lc_noc TO itg_salida-tipo_doc.
    ENDIF.

    " Fin Requerimiento SD-3428 RAR


    APPEND itg_salida .
    CLEAR itg_salida.
  ENDLOOP.
ENDFORM. " F_TOTALIZA
*&---------------------------------------------------------------------*
*&      Form  INIT_BOSTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_bosta .

  IF xpend CN no_entry.
    x_bosta-sign   = 'I'.
    x_bosta-option = 'EQ'.
    x_bosta-low    = space.
    APPEND x_bosta.
  ENDIF.
*
  IF xverif CN no_entry.
    x_bosta-sign   = 'I'.
    x_bosta-option = 'EQ'.
    x_bosta-low    = 'A'.
    APPEND x_bosta.
  ENDIF.
*
  IF xliberac CN no_entry.
    x_bosta-sign   = 'I'.
    x_bosta-option = 'EQ'.
    x_bosta-low    = 'B'.
    APPEND x_bosta.
  ENDIF.
*
  IF xliq CN no_entry.
    x_bosta-sign   = 'I'.
    x_bosta-option = 'EQ'.
    x_bosta-low    = 'C'.
    APPEND x_bosta.
  ENDIF.
*
  IF xabono CN no_entry.
    x_bosta-sign   = 'I'.
    x_bosta-option = 'EQ'.
    x_bosta-low    = 'D'.
    APPEND x_bosta.
  ENDIF.

ENDFORM. " INIT_BOSTA
*&---------------------------------------------------------------------*
*&      Form  USER COMMAND
*&---------------------------------------------------------------------*
* -------------------------------------------------- ALV USER COMMAND
FORM user_command USING r_ucomm LIKE sy-ucomm
                          rs_selfield TYPE  slis_selfield.
**
  DATA:   wa_alv  LIKE itg_salida.
**
  CASE r_ucomm.
*******************************************************************
****************************************************** DOUBLE CLICK
*******************************************************************
    WHEN '&IC1'.
**
      CHECK NOT rs_selfield-value IS INITIAL.
**--- Busca en la tabla el acuerdo seleccionado
      READ TABLE itg_salida INDEX rs_selfield-tabindex INTO wa_alv.

      IF  rs_selfield-sel_tab_field CS 'KNUMA'.
**--- Se muestra el acuerdo seleccionado
        SET PARAMETER ID 'VBO' FIELD wa_alv-knuma.
        CALL TRANSACTION 'VBO3' AND SKIP FIRST SCREEN.
      ENDIF.
**
  ENDCASE.
ENDFORM. "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_PROVISIÓN_MANUAL
*&---------------------------------------------------------------------*
*       Rutina Cálculo de Provisión Manual
*----------------------------------------------------------------------*
FORM f_provision_manual.

  DATA: stl_kona TYPE tyg_kona,
        stl_vbrk TYPE tyg_vbrk,
        stl_konv TYPE tyg_konv,
*        stl_konp TYPE tyg_konp,
        stl_s060 TYPE tyg_s060.

  CLEAR:   stg_acuerdos,stg_paso, i_detalle.
  REFRESH: itg_acuerdos,itg_paso.
*
  DATA: vl_prov_manual TYPE kwert,
        vl_indice      TYPE i.
*
  LOOP AT i_detalle.
    MOVE-CORRESPONDING i_detalle TO stg_paso.
    MOVE-CORRESPONDING i_detalle TO stg_acuerdos.
    APPEND stg_acuerdos TO itg_acuerdos.
    APPEND stg_paso TO itg_paso.
  ENDLOOP.
*
  SORT itg_acuerdos BY knuma DESCENDING.
  DELETE ADJACENT DUPLICATES FROM itg_acuerdos COMPARING knuma.
*
  SORT itg_paso BY knumh DESCENDING.
  DELETE ADJACENT DUPLICATES FROM itg_paso COMPARING knuma knumh.
*
  IF itg_paso IS NOT INITIAL.
    REFRESH itg_s060.
    CLEAR itg_s060.
    SELECT knumh auwrt_k rrwrt_k kwaeh kawrt_k kstbs INTO CORRESPONDING FIELDS OF TABLE itg_s060
    FROM s060
    FOR ALL ENTRIES IN itg_paso                    "#EC CI_NO_TRANSFORM
    WHERE knumh EQ itg_paso-knumh.
  ENDIF.
*
  CLEAR: stg_acuerdos,itg_salida,stg_paso, stg_tsum.

  LOOP AT itg_acuerdos INTO stg_acuerdos.

    CLEAR: itg_salida, stg_tsum.
    LOOP AT itg_salida WHERE knuma = stg_acuerdos-knuma. "x4
      ADD itg_salida-kwert_k TO stg_tsum-kwert_k. " Total peso

    ENDLOOP.

    CLEAR stg_paso.
    LOOP AT itg_paso INTO stg_paso WHERE knuma = stg_acuerdos-knuma.

      MOVE stg_paso-knuma TO stg_tsum-knuma.  "Núm.Acuerdo Rapel
      MOVE stg_paso-kschl TO stg_tsum-kschl.  "Clase de Condición
      MOVE stg_paso-knumh TO stg_tsum-knumh.  "Nº Registro Cond.

      CLEAR itg_s060.
      LOOP AT itg_s060 WHERE knumh = stg_paso-knumh.
        stg_tsum-auwrt_k  = stg_tsum-auwrt_k + itg_s060-auwrt_k. "Pago rappel
        stg_tsum-rrwrt_k  = stg_tsum-rrwrt_k + itg_s060-rrwrt_k. "Atrasos liquidados
      ENDLOOP.

    ENDLOOP.
    APPEND stg_tsum TO itg_tsum.
    CLEAR stg_tsum.
  ENDLOOP.
*
  CLEAR: stg_tsum.
  LOOP AT itg_tsum INTO stg_tsum.
    vl_indice = sy-tabix.

    READ TABLE itg_kona WITH KEY knuma = stg_tsum-knuma.
    IF  sy-subrc EQ 0.
      SELECT fktyp vbtyp vkorg vtweg spart kunag knumv fksto
      INTO CORRESPONDING FIELDS OF TABLE itg_vbrk2 FROM vbrk
      WHERE fktyp EQ cg_k
      AND vbtyp NE cg_s
      AND vkorg EQ itg_kona-vkorg
      AND vtweg EQ itg_kona-vtweg
      AND spart EQ itg_kona-spart
      AND kunag EQ itg_kona-bonem
      AND knuma EQ itg_kona-knuma
      AND fksto EQ space.
    ENDIF.
*
    IF itg_vbrk2[] IS NOT INITIAL.

      SELECT knumv kschl knumh kwert INTO CORRESPONDING FIELDS OF TABLE itg_konv2 FROM konv
        FOR ALL ENTRIES IN itg_vbrk2               "#EC CI_NO_TRANSFORM
         WHERE knumv EQ itg_vbrk2-knumv
         AND   kschl EQ itg_salida-boart.

      IF  sy-subrc EQ 0.
        CLEAR vl_prov_manual.
        LOOP AT itg_konv2 INTO stl_konv.

          vl_prov_manual = vl_prov_manual + stl_konv-kwert.

          AT LAST.
*- Provisión manual
            stg_tsum-ruwrt_m = vl_prov_manual + stl_konv-kwert.
          ENDAT.

        ENDLOOP.
*- Volumen de negocio
        IF ( stg_tsum-ruwrt_m EQ 0 ) AND ( stg_tsum-kwert_k EQ 0 ).
          stg_tsum-volum = 0.
        ELSE.
          stg_tsum-volum = stg_tsum-kwert_k - stg_tsum-auwrt_k - stg_tsum-ruwrt_m.
        ENDIF.

      ELSE.
        stg_tsum-ruwrt_m = 0.
      ENDIF.

    ELSE.
*- Volumen de negocio
      IF stg_tsum-kwert_k EQ 0.
        stg_tsum-volum = 0.
      ELSE.
        stg_tsum-volum = stg_tsum-kwert_k - stg_tsum-auwrt_k.
      ENDIF.

    ENDIF.

    MODIFY itg_tsum FROM stg_tsum INDEX vl_indice.

  ENDLOOP.
*
*  DATA: wa_cellcolor TYPE lvc_s_scol.
  DATA: vl_encontro_cantidad TYPE char1.

  CLEAR: stg_acuerdos, stg_tsum, itg_salida,lw_bold.
  LOOP AT itg_acuerdos INTO stg_acuerdos.

    LOOP AT itg_paso INTO stg_paso WHERE knuma = stg_acuerdos-knuma.

      LOOP AT itg_tsum INTO stg_tsum WHERE knuma EQ stg_paso-knuma.

        CLEAR: itg_salida, vl_encontro_cantidad.
        LOOP AT itg_salida WHERE knuma EQ stg_tsum-knuma.
          IF itg_salida-rrwrt_k IS NOT INITIAL.
            vl_encontro_cantidad = 'X'.
            EXIT.
          ELSE.
            vl_encontro_cantidad = ''.
          ENDIF.
        ENDLOOP.

        CLEAR itg_salida.
        itg_salida-knuma    = stg_tsum-knuma.    "Acuerdo
*        itg_salida-ruwrt_m  = stg_tsum-ruwrt_m.  "Provisión Manual
        IF vl_encontro_cantidad IS INITIAL.
          itg_salida-rrwrt_k  = stg_tsum-rrwrt_k.       "Atrasos liquidados
          itg_salida-auwrt_k  = stg_tsum-rrwrt_k * -1.  "Pago rappel
        ENDIF.

        itg_salida-volum    = stg_tsum-volum  .  "Volumen de negocios
*    wa_cellcolor-fname = 'RUWRT_M'.
*    wa_cellcolor-color-col = sy-tabix.  "color code 1-7, if outside rage defaults to 7
*    wa_cellcolor-color-int = '1'.  "1 = Intensified on, 0 = Intensified off
*    wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
*   APPEND wa_cellcolor TO itg_salida-cellcolor.
        itg_salida-tipo_doc = 'Sub.totales'.
        itg_salida-color = 'C110'.

        lw_bold-style = '00000121'. "Negrita
        lw_bold-fieldname = 'TIPO_DOC'.
        INSERT lw_bold INTO lt_bold INDEX 1.
        itg_salida-bold = lt_bold.
        FREE lt_bold.
      ENDLOOP.
    ENDLOOP.

    APPEND itg_salida.
    CLEAR itg_salida.
  ENDLOOP.


  SORT itg_salida ASCENDING BY knuma tipo_doc.

ENDFORM. " F_PROVISIÓN_MANUAL
*&---------------------------------------------------------------------*
*&      Form  f_provision_manual_detalle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_provision_manual_detalle.
  CLEAR: itg_salida,stg_vbrk_abonos,stg_vbrp_abonos.
*
*** provisión manual por documento
  IF itg_kona[] IS NOT INITIAL.

    REFRESH itg_vbrk_abonos[].
    SELECT vbeln knuma vbtyp knumv INTO CORRESPONDING FIELDS OF TABLE itg_vbrk_abonos
    FROM vbrk
    FOR ALL ENTRIES IN itg_kona                    "#EC CI_NO_TRANSFORM
    WHERE knuma EQ itg_kona-knuma AND
          fkart EQ  cg_za51.
  ENDIF.
*
  IF itg_vbrk_abonos[] IS NOT INITIAL.

    REFRESH itg_vbrp_abonos[].
    SELECT vbeln posnr netwr matnr INTO CORRESPONDING FIELDS OF TABLE itg_vbrp_abonos
      FROM vbrp
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
      WHERE vbeln EQ itg_vbrk_abonos-vbeln.
  ENDIF.
*

  IF itg_vbrk_abonos[] IS NOT INITIAL.

    REFRESH itg_konv3[].
    SELECT knumv kposn kschl knumh kwert INTO CORRESPONDING FIELDS OF TABLE itg_konv3
      FROM konv
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
      WHERE knumv EQ itg_vbrk_abonos-knumv AND
*             kschl EQ cg_zarh AND
            kschl IN (cg_zarh, cg_zarg, cg_zarf) AND
            kruek EQ 'X'.
  ENDIF.

*
  LOOP AT itg_kona.
    LOOP AT itg_vbrk_abonos INTO stg_vbrk_abonos WHERE knuma = itg_kona-knuma.
      LOOP AT itg_vbrp_abonos INTO stg_vbrp_abonos  WHERE vbeln = stg_vbrk_abonos-vbeln.

        CLEAR i_detalle.
        i_detalle-knuma   = stg_vbrk_abonos-knuma.
        i_detalle-bonem   = itg_kona-bonem.
        i_detalle-boart   = itg_kona-boart.
        i_detalle-datab   = itg_kona-datab.
        i_detalle-datbi   = itg_kona-datbi.
        i_detalle-waers   = itg_kona-waers.
        i_detalle-vbeln   = stg_vbrp_abonos-vbeln.
        i_detalle-posnr   = stg_vbrp_abonos-posnr.
        i_detalle-matnr   = stg_vbrp_abonos-matnr.
        i_detalle-pmanual = 'X'.

        CLEAR stg_konv3.
        READ TABLE itg_konv3 INTO stg_konv3 WITH KEY knumv = stg_vbrk_abonos-knumv
                                                     kposn = stg_vbrp_abonos-posnr
                                                     kschl = itg_kona-boart.
        IF sy-subrc EQ 0.
          i_detalle-ruwrt_m = stg_konv3-kwert. "Provisiones Manuales
        ENDIF.

        APPEND i_detalle.
        CLEAR i_detalle.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_liquidacion_parcial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_liquidacion_parcial.
*
*** Liquidación parcial
  IF itg_kona[] IS NOT INITIAL.
    REFRESH itg_vbrk_abonos[].
    SELECT vbeln knuma vbtyp knumv INTO CORRESPONDING FIELDS OF TABLE itg_vbrk_abonos
      FROM vbrk
      FOR ALL ENTRIES IN itg_kona                  "#EC CI_NO_TRANSFORM
      WHERE knuma EQ itg_kona-knuma AND
            fkart EQ cg_za67.
  ENDIF.
*
  IF itg_vbrk_abonos[] IS NOT INITIAL.
    REFRESH itg_vbrp_abonos[].
    SELECT vbeln posnr netwr matnr INTO CORRESPONDING FIELDS OF TABLE itg_vbrp_abonos
      FROM vbrp
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
      WHERE vbeln EQ itg_vbrk_abonos-vbeln.
  ENDIF.
*
  IF itg_vbrk_abonos[] IS NOT INITIAL.
    REFRESH itg_konv3[].
    SELECT knumv kposn kschl knumh kwert INTO CORRESPONDING FIELDS OF TABLE itg_konv3
      FROM konv
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
      WHERE knumv EQ itg_vbrk_abonos-knumv AND
*            kschl EQ cg_zarh AND
            kschl IN (cg_zarh, cg_zarg, cg_zarf) AND
            kruek NE 'X'.
  ENDIF.

***
  LOOP AT itg_kona.
    LOOP AT itg_vbrk_abonos INTO stg_vbrk_abonos WHERE knuma = itg_kona-knuma.
      LOOP AT itg_vbrp_abonos INTO stg_vbrp_abonos  WHERE vbeln = stg_vbrk_abonos-vbeln.

        CLEAR i_detalle.
        i_detalle-knuma   = stg_vbrk_abonos-knuma.
        i_detalle-bonem   = itg_kona-bonem.
        i_detalle-boart   = itg_kona-boart.
        i_detalle-datab   = itg_kona-datab.
        i_detalle-datbi   = itg_kona-datbi.
        i_detalle-waers   = itg_kona-waers.
        i_detalle-vbeln   = stg_vbrp_abonos-vbeln.
        i_detalle-posnr   = stg_vbrp_abonos-posnr.
        i_detalle-matnr   = stg_vbrp_abonos-matnr.
        i_detalle-lparcial = 'X'.

        CLEAR stg_konv3.
        READ TABLE itg_konv3 INTO stg_konv3 WITH KEY knumv = stg_vbrk_abonos-knumv
                                                     kposn = stg_vbrp_abonos-posnr
                                                     kschl = itg_kona-boart.
        IF sy-subrc EQ 0.
          i_detalle-rrwrt_k = stg_konv3-kwert. "Liquidación parcial
        ENDIF.

        APPEND i_detalle.
        CLEAR i_detalle.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_base_condicion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_base_condicion .
  DATA: vl_tabix TYPE sy-tabix.
*
  IF i_detalle[] IS NOT INITIAL.

    REFRESH itg_vbrk_abonos[].
    SELECT vbeln knuma vbtyp knumv INTO CORRESPONDING FIELDS OF TABLE itg_vbrk_abonos
      FROM vbrk
      FOR ALL ENTRIES IN i_detalle                 "#EC CI_NO_TRANSFORM
      WHERE vbeln EQ i_detalle-vbeln.
  ENDIF.
*
  IF itg_vbrk_abonos[] IS NOT INITIAL.

    REFRESH itg_vbrp_abonos[].
    SELECT vbeln posnr netwr matnr INTO CORRESPONDING FIELDS OF TABLE itg_vbrp_abonos
      FROM vbrp
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
      WHERE vbeln EQ itg_vbrk_abonos-vbeln.
  ENDIF.
*
  IF i_detalle[] IS NOT INITIAL.

    REFRESH itg_konp[].
    SELECT knumh krech INTO CORRESPONDING FIELDS OF TABLE itg_konp
      FROM konp
      FOR ALL ENTRIES IN i_detalle
      WHERE knumh EQ i_detalle-knumh.
  ENDIF.
*
  IF itg_vbrk_abonos[] IS NOT INITIAL.
    REFRESH itg_konv3[].
    SELECT knumv kposn kschl knumh kwert kawrt kstbs INTO CORRESPONDING FIELDS OF TABLE itg_konv3
      FROM konv
      FOR ALL ENTRIES IN itg_vbrk_abonos           "#EC CI_NO_TRANSFORM
       WHERE knumv EQ itg_vbrk_abonos-knumv AND
*             kschl EQ cg_zarh AND
             kschl IN (cg_zarh, cg_zarg, cg_zarf).
  ENDIF.

  LOOP AT i_detalle.
*
    CLEAR vl_tabix.
    vl_tabix = sy-tabix.
*
    CLEAR stg_vbrk_abonos.
    READ TABLE itg_vbrk_abonos INTO stg_vbrk_abonos WITH KEY vbeln = i_detalle-vbeln.
*
    CLEAR stg_konv3.
    READ TABLE itg_konv3 INTO stg_konv3 WITH KEY knumv = stg_vbrk_abonos-knumv
                                                 kposn = i_detalle-posnr
                                                 kschl = itg_kona-boart.
*
    CLEAR stg_konp.
    READ TABLE itg_konp INTO stg_konp WITH KEY knumh = i_detalle-knumh.
*
    MOVE stg_konv3-kawrt TO i_detalle-kawrt_k.
    MOVE stg_konv3-kstbs TO i_detalle-kstbs.
*
    IF stg_konp-krech = 'B'.
      i_detalle-kawrt_k = 0.
    ENDIF.
    MODIFY i_detalle INDEX  vl_tabix.

  ENDLOOP.


ENDFORM.

*Text elements
*----------------------------------------------------------
* 001 Agreement
* 002 Rebate recipient
* 003 Provision
* 004 Currency
* 005 rappel payment
* 006 Currency
* 007 delays settled
* 008 Currency
* 009 Kind agreement
* 010 Validity
* 011 End of validity
* 012 Basis for condition
* 013 Base escale
* 014 Volumen de Negocios
* T01 Selection parameters
* T02 Rebate settlement status
* T03 There is no information require
* TI1 Report agreement rebate
* TI2 Date
* TI3 Time
* TI4 User


*Selection texts
*----------------------------------------------------------
* P_DATBI         end of validity of the agreeme
* S_BONEM         Rebate recipient
* S_IDENT3         Settlement period
* S_KNUMA         Agreement
* S_SPART         Sector
* S_VKORG         Sales Organization
* S_VTWEG         Distribution Channel
* XABONO         Credit memo req.created
* XLIBERAC         Agreement released
* XLIQ         Final settl.carried out
* XPEND         Open
* XVERIF         Agreement to be checked


*Messages
*----------------------------------------------------------
*
* Message class: ZI
*215   You do not have access to the sales organization &
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2019. Sap Release 750