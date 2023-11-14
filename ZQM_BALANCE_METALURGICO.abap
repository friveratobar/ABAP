***==================================================================***
*& / \ / Report  zmaqr004 / \ /
***==================================================================***
*& Compañia   : *******                                                *
*& Autor      : Felipe Rivera                                          *
*& Usuario    : *******                                                *
*& Funcional  : *******                                                *
*& Fecha      : 15/09/2014                                             *
*& Objetivo   : Mostar Reportes - Balance metalúrgico mensual          *
***==================================================================***
REPORT zmaqr004 NO STANDARD PAGE HEADING MESSAGE-ID zz.

INCLUDE zmaqr004_top.
***==================================================================***
*       CLASS lcl_event_receiver DEFINITION
*
*       Objetivo: definir clase, para dar funcionalidad a eventos de alv
***==================================================================***
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
          handle_hotspot_click
          FOR EVENT hotspot_click OF cl_gui_alv_grid
          IMPORTING e_row_id e_column_id es_row_no.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION
***==================================================================***
*        Class (Implementation)  lcl_event_receiver
*
*        Objetivo: implementar clase, para dar funcionalidad a eventos de alv
***==================================================================***
CLASS lcl_event_receiver IMPLEMENTATION.
*Handle hostpot un click

* Handle hotspot click
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_end_of_list

ENDCLASS.               "lcl_event_receiver

* Referenciar la clase de eventos.
DATA: g_event_receiver    TYPE REF TO lcl_event_receiver.
*
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

END-OF-SELECTION.

CLEAR: e_days,e_months,e_years.
* Cantidad de meses y años dentro de una rango de fechas
CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
  EXPORTING
    i_date_from         = s_wadat-low
    i_date_to           = s_wadat-high
 IMPORTING
   e_days               = e_days
   e_months             = e_months
   e_years              = e_years.

IF e_years  <= 1.
  IF  e_months <= 12.
  ELSE.
    MESSAGE 'El rango maximo son 12 meses' TYPE 'S'.
    EXIT.
   ENDIF.
ELSE.
   MESSAGE 'Rango maximo 12 meses' TYPE 'S'.
    EXIT.
ENDIF.
***==================================================================***
**                 Carga y obtención de datos
***==================================================================***
PERFORM filtro_entregas. " --> Filtro de entregas mov 641 ,mov 101
PERFORM load_ddeli.      " --> Carga Diario  Delivery
PERFORM load_deli.       " --> Carga Mensual Delivery
PERFORM load_dprov.      " --> Carga Diaro return provisional
PERFORM load_prov.       " --> Carga Mensual provisional
PERFORM load_dfin.       " --> Carga Diario return final
PERFORM load_fin.        " --> Carga Return final
PERFORM load_diff.       " --> Carga Difference (Provisional-Final)
PERFORM load_pddeli.     " --> Carga Penalties diario delivery
PERFORM load_pdeli.      " --> Carga Penalties mensual delivery
PERFORM load_pdprov.     " --> Carga Penalties diario provisional
PERFORM load_pprov.      " --> Carga Penalties mensual provisional
PERFORM load_pdfin.      " --> Carga Penalties diario final
PERFORM load_pfin.       " --> Carga Penalties mensual final
PERFORM load_dvolume.    " --> Carga Diario volumen
PERFORM load_volume.     " --> Carga Volumen mensual delivery
PERFORM load_dvolfin.    " --> Carga Volumen diario final
PERFORM load_volfin.     " --> Carga Volumen mesual final
***==================================================================***
**                 LLamada screen padre
***==================================================================***
CALL SCREEN 9000.

FORM filtro_entregas.

* Filtro entregas
        SELECT a~vbeln           " Entrega
               a~wadat_ist       " Fecha movimiento de mercancías real
               a~wadat           " Fecha prevista para movimiento de mercancías
               a~kunnr           " Destinatario de mercancías
               b~wbstk           " Status total de movimiento de mercancías
    INTO TABLE gt_likp FROM likp AS a
    INNER JOIN vbuk AS b ON b~vbeln EQ a~vbeln
         WHERE a~wadat_ist  IN s_wadat AND " Fecha
               a~lfart      EQ  'ZNL1' AND " Filtro maquila
               b~wbstk      EQ  'C'.       " Estatus concluido
SORT gt_likp[] BY vbeln.

* Obtengo las posiciones
 IF gt_likp[] IS NOT INITIAL.
         SELECT vbeln
                posnr
                matnr
                charg
                lgort
     INTO TABLE gt_lips FROM lips FOR ALL ENTRIES IN gt_likp
          WHERE vbeln   EQ gt_likp-vbeln AND
                charg <> space.
ENDIF.

* Obtengo mov. 641 y 101.
IF gt_lips[] IS NOT INITIAL.
         SELECT mblnr
                bwart
                werks
                lgort
                charg
                vbeln_im
                vbelp_im
                budat_mkpf
     INTO TABLE gt_msegx FROM mseg FOR ALL ENTRIES IN gt_lips
          WHERE vbeln_im  EQ gt_lips-vbeln
            AND vbelp_im  EQ gt_lips-posnr
            AND charg     EQ gt_lips-charg
            AND lgort     EQ gt_lips-lgort
            AND bwart     IN ('641','101').
           SORT gt_msegx BY vbeln_im vbelp_im.
ENDIF.

   aux_lips[] = gt_lips[].
   REFRESH gt_lips[].

 LOOP AT aux_lips ASSIGNING <lx_lips>.
   CLEAR: ls_msegx.
   READ TABLE gt_msegx INTO ls_msegx WITH KEY vbeln_im = <lx_lips>-vbeln
                                              vbelp_im = <lx_lips>-posnr
                                              bwart    = '641' BINARY SEARCH.
   IF sy-subrc EQ 0.
     ls_lips-vbeln      = <lx_lips>-vbeln.
     ls_lips-posnr      = <lx_lips>-posnr.
     ls_lips-matnr      = <lx_lips>-matnr.
     ls_lips-charg      = <lx_lips>-charg.
     ls_lips-lgort      = <lx_lips>-lgort.
     ls_lips-mblnr      = ls_msegx-mblnr.
     ls_lips-budat_mkpf = ls_mseg-budat_mkpf.

     APPEND ls_lips TO gt_lips.

   ENDIF.
ENDLOOP.

*    SELECT  ps~vbeln          " Entrega
*            ps~posnr          " Posicion
*            ps~matnr          " Material
*            ps~charg          " Número de lote
*            eg~mblnr          " Número de documento material
*            eg~budat_mkpf     " Fecha de contabilización en el documento
* INTO TABLE gt_lips
*       FROM lips  AS ps
* INNER JOIN mseg AS eg
*         ON eg~vbeln_im EQ ps~vbeln
*                       AND eg~vbelp_im  EQ ps~posnr
*                       AND eg~charg     EQ ps~charg
*                       AND eg~lgort     EQ ps~lgort
*        FOR ALL ENTRIES IN gt_likp
*                     WHERE
*      " ps~charg    IN s_charg AND
*                           ps~vbeln     EQ gt_likp-vbeln
*                       AND eg~bwart     EQ '641'.

*IF gt_likp[] IS NOT INITIAL.
*    SELECT  ps~vbeln          " Entrega
*            ps~posnr          " Posicion
*            ps~matnr          " Material
*            ps~charg          " Número de lote
*            eg~mblnr          " Número de documento material
*            eg~budat_mkpf     " Fecha de contabilización en el documento
*      INTO  CORRESPONDING FIELDS OF TABLE gt_m101
*      FROM  lips  AS ps
*            INNER JOIN mseg AS eg
*                    ON eg~vbeln_im EQ ps~vbeln
*                   AND eg~vbelp_im EQ ps~posnr
*                   AND eg~charg    EQ ps~charg
*                 " AND eg~lgort       EQ ps~lgort
*      FOR   ALL ENTRIES IN gt_likp
*      WHERE
*      "ps~charg    "IN s_charg
*        "AND
*            ps~vbeln EQ gt_likp-vbeln
*        AND eg~bwart EQ '101'.
**        AND eg~werks    IN r_werks
**        AND eg~lgort    IN r_lgort
**        AND eg~matnr    IN r_matnr.
*SORT gt_m101 BY vbeln posnr.
*ENDIF.

*    aux_lips[] = gt_lips[].
*    REFRESH gt_lips[].

 LOOP AT aux_lips ASSIGNING <lx_lips>.

   CLEAR: ls_msegx.
   READ TABLE gt_msegx INTO ls_msegx WITH KEY vbeln_im = <lx_lips>-vbeln
                                              vbelp_im = <lx_lips>-posnr
                                              bwart    = '101' BINARY SEARCH.
   IF sy-subrc EQ 0.
     ls_m101-vbeln      = <lx_lips>-vbeln.
     ls_m101-posnr      = <lx_lips>-posnr.
     ls_m101-matnr      = <lx_lips>-matnr.
     ls_m101-charg      = <lx_lips>-charg.
     ls_m101-lgort      = <lx_lips>-lgort.
     ls_m101-mblnr      = ls_msegx-mblnr.
     ls_m101-budat_mkpf = ls_msegx-budat_mkpf.
     APPEND ls_m101 TO gt_m101.

   ENDIF.
ENDLOOP.

 SELECT setname valsign valoption valfrom valto
   FROM setleaf INTO TABLE gt_setleaf
  WHERE setname EQ 'ZQM_MATERIAL'.

   IF gt_setleaf[] IS NOT INITIAL.
* Carga rango material terminado
    LOOP AT gt_setleaf INTO ls_setleaf.
      w_material-sign   = 'I'.
      w_material-option = 'EQ'.
      w_material-low = ls_setleaf-valfrom.
      APPEND w_material TO r_material.
    ENDLOOP.
   ENDIF.

* Obtenemos los mov101 del Producto terminado( 160 )
       SELECT   mblnr
                bwart
                matnr
                werks
                lgort
                charg
                menge
                meins
                erfmg
                erfme
                budat_mkpf
     INTO TABLE gt_mseg    FROM mseg
          WHERE budat_mkpf IN s_wadat    AND
                bwart      EQ '101'      AND
                matnr      IN r_material AND
                werks      EQ '8300'.
ENDFORM.

FORM  load_deli.

  CLEAR: lv_date, lv_final_date.
  lv_date = s_wadat-low.
  ls_rep-month = lv_date+0(6).
  APPEND ls_rep TO gt_rep.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.
   ls_rep-month    = lv_final_date+0(6).
   lv_date         = lv_final_date.
   APPEND ls_rep TO gt_rep.

  ENDDO.

* CLEAR: lv_ano , lv_mes , lv_anomes.
* Cargo los meses
* lv_ano = s_wadat-low+0(4).
*  DO e_months TIMES.
*    ADD 01 TO lv_mes.
*
*    CONCATENATE lv_ano lv_mes INTO lv_anomes.
*    ls_rep-month = lv_anomes.
*
*   IF lv_anomes >= s_wadat-low+0(6) AND
*      lv_anomes <=  s_wadat-high+0(6).
*      APPEND ls_rep TO gt_rep.
*   ELSE.
*     CONTINUE.
*   ENDIF.
*  ENDDO.

* Acumulo los valores por mes
LOOP AT gt_rep INTO ls_rep.
    CLEAR: gv_tabix, lv_factor.
  gv_tabix = sy-tabix.

 LOOP AT gt_ddeli INTO ls_ddeli WHERE dias+0(6) EQ ls_rep-month.

   ADD ls_ddeli-net        TO ls_rep-netwe.
   ADD ls_ddeli-dry        TO ls_rep-drywe.
   ADD ls_ddeli-mo_grade   TO ls_rep-mo_grade.
   ADD ls_ddeli-mo_content TO ls_rep-mo_content.
   ADD ls_ddeli-re_rate    TO ls_rep-re_rate.
   ADD ls_ddeli-mo_return  TO ls_rep-mo_return.
   ADD ls_ddeli-pro_fee    TO ls_rep-pro_fee.
   ADD ls_ddeli-pro_fee_us TO ls_rep-pro_fee_us.
   ADD ls_ddeli-ave_return TO ls_rep-ave_return.
   ADD ls_ddeli-delivery   TO ls_rep-delivery.
   ADD 1 TO lv_factor.

 ENDLOOP.

     ls_rep-mo_grade = ls_rep-mo_grade / lv_factor.
     ls_rep-re_rate  = ls_rep-re_rate  / lv_factor.
     MODIFY gt_rep FROM ls_rep INDEX gv_tabix.

 ENDLOOP.

LOOP AT gt_rep INTO ls_rep.
    gv_tabix = sy-tabix.
    gv_tabix2 = gv_tabix - 1.

    CLEAR: ls_rep2.
    READ TABLE gt_rep INTO ls_rep2 INDEX gv_tabix2.

    ls_rep-accumulation = ls_rep-delivery + ls_rep2-accumulation.

    MODIFY gt_rep FROM ls_rep INDEX gv_tabix.
ENDLOOP.

ENDFORM.
***==================================================================***
*                  FORM load_dprov.
*&---------------------------------------------------------------------*
*                  obj. Cargar gt_dprov
***==================================================================***
FORM load_dprov.

  LOOP AT gt_m101 ASSIGNING <lx_m101>.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = <lx_m101>-vbeln BINARY SEARCH.

   ls_dprov-dias  = ls_likp-wadat_ist.
   ls_dprov-vbeln = ls_likp-vbeln.
   ls_dprov-charg = <lx_m101>-charg.
   ls_dprov-matnr = <lx_m101>-matnr.

*   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*   EXPORTING
*     input = ls_m101-matnr
*   IMPORTING
*     output = ls_dprov-matnr .

      CLEAR:  lv_bag,      " Numero de Bags (MaxiSacos)
              lv_gross,    " Gross Weight (KG)
              lv_tare,     " Tare (KG)
              lv_net,      " Net Weight (KG)
              lv_moisture, " Moisture (%)
              lv_dry.      " Dry Weight (KG)

          CALL FUNCTION 'ZQM012_PESOS'
            EXPORTING
              i_entrega = <lx_m101>-vbeln
              i_matnr   = <lx_m101>-matnr
              i_lote    = <lx_m101>-charg
              i_bwart   = '101'
            IMPORTING
              gross     = lv_gross
              bag       = lv_bag
              tare      = lv_tare
              moisture  = lv_moisture
              net       = lv_net
              dry       = lv_dry.

       ls_dprov-gross   = lv_gross.        " Gross Weight (KG)
       ls_dprov-bag     = lv_bag.          " Numero de bultos
       ls_dprov-tara    = lv_tare.         " Tare (KG)
       ls_dprov-net     = lv_net.          " Net Weight (KG
       ls_dprov-humedad = lv_moisture.     " Humedad(%)
       ls_dprov-dry     = lv_dry.          " Dry Weight (KG)

* CLEAR: lv_objectkey.
* CONCATENATE <lx_m101>-matnr <lx_m101>-charg INTO lv_objectkey.

** Obtengo las caracteristicas de lote
* CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*     EXPORTING
*       objectkey              = lv_objectkey
*       objecttable            = 'MCH1'
*       classnum               = 'CONCENTRADO_MO'
*       classtype              = '023'
*       keydate                = sy-datum
*       language               = sy-langu
*     TABLES
*       allocvaluesnum         = t_num
*       allocvalueschar        = t_char
*       allocvaluescurr        = t_curr
*       return                 = t_return.
*
* IF sy-subrc EQ 0.
*** Ley Mo
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'LEY_MO'.
*  IF sy-subrc EQ 0.
*      ls_dprov-ley_mo = ls_num-value_from.
*  ENDIF.
* ENDIF.


CLEAR: lv_cuobj_bm,lv_ca , lv_sodio ,lv_arsenico.
SELECT SINGLE cuobj_bm INTO lv_cuobj_bm
         FROM mch1
        WHERE matnr EQ <lx_m101>-matnr AND
              charg EQ <lx_m101>-charg.

     REFRESH: lt_ausp[].
      SELECT a~atinn a~atwrt a~atflv  a~atflb c~atnam c~atfor
  INTO TABLE lt_ausp
        FROM ausp AS a INNER JOIN
             cabn AS c ON c~atinn EQ a~atinn
       WHERE a~objek EQ lv_cuobj_bm AND
             c~atfor EQ 'NUM'.

 IF sy-subrc EQ 0.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'LEY_MO'.
   IF sy-subrc EQ 0.
      ls_dprov-ley_mo = ls_ausp-atflv.
   ENDIF.
 ENDIF.

* recovery rate
  CLEAR: lv_ebeln.
*~~> Se Obtiene Plan de Entregas y contrato Asociado.
  SELECT SINGLE ebeln INTO lv_ebeln FROM ekbe
          WHERE belnr EQ <lx_m101>-vbeln.

  CLEAR: lv_ihrez.
  SELECT SINGLE ihrez INTO lv_ihrez FROM ekko
          WHERE ebeln EQ  lv_ebeln.

  CLEAR: ls_molib_002.
  SELECT SINGLE * INTO  ls_molib_002 FROM zmm_molib_002
          WHERE contnr EQ lv_ihrez AND
                plane  EQ lv_ebeln.

  CLEAR: lv_mo_kg, lv_mo_lb.
*** Mo content (kg)
  CLEAR: lv_cantidad.
  lv_cantidad = ls_dprov-dry.

  CALL FUNCTION 'ZSD_CALCULO_MO_FINO_V2'
    EXPORTING
      material       = <lx_m101>-matnr
      lote           = <lx_m101>-charg
      cantidad       = lv_cantidad
      um             = 'KG'
      caract_finales = ''
    IMPORTING
      mo_fino_kg     = lv_mo_kg
      mo_fino_lb     = lv_mo_lb.

* Mo content(kg).(Dry weight * Ley MO)
    ls_dprov-mo_content    = lv_mo_kg.

   CLEAR:lv_penal,lv_dias_retorno.
   CALL FUNCTION 'ZQM013_PENALIDADES'
     EXPORTING
       i_lote          = <lx_m101>-charg
       i_matnr         = <lx_m101>-matnr
    IMPORTING
      p_total          = lv_penal
      p_t_dias_retorno = lv_dias_retorno.

   IF ls_molib_002 IS NOT INITIAL.
*** Tasa de recuperación
    ls_dprov-re_rate  = ls_molib_002-rrate." valor fijo

*** Processing Fee (US$/lb Mo)
   ls_dprov-pro_fee   = ls_molib_002-pefee + lv_penal.
   ENDIF.

*** Mo returnable (kg) (Mo Content (kg) * recovery rate)
   ls_dprov-mo_return =  ls_dprov-mo_content * ls_dprov-re_rate.

*** Processing Fee (US$)
   ls_dprov-pro_fee_us  = ls_dprov-mo_return * lv_lb * ls_dprov-pro_fee.
   "* ls_eli-pro_fee.

*** Mo grade ( Mo content (kg) / Dry weight)
   IF ls_dprov-dry <> 0.
   ls_dprov-mo_grade = ls_dprov-mo_content / ls_dprov-dry.
   ENDIF.

*** Average_return
   ls_dprov-ave_return = ls_dprov-mo_content * ( ls_molib_002-peret + lv_dias_retorno ).

* LOOP AT gt_ddeli INTO <lx_ddeli>.
*   gv_tabix  = sy-tabix.
*   gv_tabix2 = gv_tabix - 1.
*
* CLEAR: ls_ddeli2.
* READ TABLE gt_ddeli INTO ls_ddeli2 INDEX gv_tabix2.
*
*  ls_ddeli-delivery = ls_ddeli-gross.
*  ls_ddeli-accumulation = ls_ddeli-delivery + ls_ddeli2-accumulation.
*
*  MODIFY gt_ddeli FROM ls_ddeli INDEX gv_tabix.
* ENDLOOP.
   APPEND ls_dprov TO gt_dprov.
  ENDLOOP.

*** MO_OXIDE
    LOOP AT gt_dprov INTO ls_dprov.
      CLEAR: gv_tabix .
      gv_tabix = sy-tabix.

      CLEAR: ls_mseg.
      READ TABLE gt_mseg INTO ls_mseg WITH KEY budat_mkpf = ls_dprov-dias.
      IF ls_mseg IS NOT INITIAL.
        ls_dprov-mo_oxide = ls_mseg-menge.
      ENDIF.

     MODIFY gt_dprov FROM ls_dprov INDEX gv_tabix.

    ENDLOOP.

  LOOP AT gt_mseg INTO ls_mseg.

   CLEAR: ls_dprov.
   READ TABLE gt_dprov INTO ls_dprov WITH KEY dias = ls_mseg-budat_mkpf.
   IF ls_dprov IS NOT INITIAL.
        "ls_dprov-mo_oxide =
   ELSE.
    ls_dprov-dias     = ls_mseg-budat_mkpf.
    ls_dprov-matnr    = ls_mseg-matnr.
    ls_dprov-charg    = ls_mseg-charg.
    ls_dprov-mo_oxide = ls_mseg-menge.
    APPEND ls_dprov TO gt_dprov.
   ENDIF.

  SORT gt_dprov ASCENDING by dias.

  ENDLOOP.

 LOOP AT gt_dprov INTO ls_dprov.
   gv_tabix  = sy-tabix.
   gv_tabix2 = gv_tabix - 1.

  CLEAR: ls_dprov2.
  READ TABLE gt_dprov INTO ls_dprov2 INDEX gv_tabix2.

  ls_dprov-mo_stock =  ( ls_dprov2-mo_stock + ls_dprov-mo_return ) - ls_dprov-mo_oxide.
  IF ls_dprov-mo_stock  < 0.
    CLEAR: ls_dprov-mo_stock .
  ENDIF.

  MODIFY gt_dprov FROM ls_dprov INDEX gv_tabix.
 ENDLOOP.


ENDFORM.
***==================================================================***
*                  FORM load_dprov.
***==================================================================***
*                  obj.    cargar gt_prov
***==================================================================***
FORM load_prov.

  CLEAR: lv_date, lv_final_date.
  lv_date = s_wadat-low.
  ls_prov-month = lv_date+0(6).

  APPEND ls_prov TO gt_prov.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
 IMPORTING
    calc_date       = lv_final_date.

    ls_prov-month = lv_final_date+0(6).
    lv_date       = lv_final_date.
    APPEND ls_prov TO gt_prov.
  ENDDO.

LOOP AT gt_prov INTO ls_prov.
  CLEAR: gv_tabix, lv_factor.
  gv_tabix = sy-tabix.

 LOOP AT gt_dprov INTO ls_dprov WHERE dias+0(6) EQ ls_prov-month.

   ADD ls_dprov-net        TO ls_prov-netwe.
   ADD ls_dprov-dry        TO ls_prov-drywe.
   ADD ls_dprov-mo_grade   TO ls_prov-mo_grade.
   ADD ls_dprov-mo_content TO ls_prov-mo_content.
   ADD ls_dprov-re_rate    TO ls_prov-re_rate.
   ADD ls_dprov-mo_return  TO ls_prov-mo_return.
   ADD ls_dprov-pro_fee    TO ls_prov-pro_fee.
   ADD ls_dprov-pro_fee_us TO ls_prov-pro_fee_us.
   ADD ls_dprov-ave_return TO ls_prov-ave_return.
   ADD ls_dprov-mo_oxide   TO ls_prov-mo_oxide.
   "ADD ls_dprov-mo_stock   TO ls_prov-mo_stock.
   ADD 1 TO lv_factor.

 ENDLOOP.
   ls_prov-mo_grade = ls_prov-mo_grade / lv_factor.
   ls_prov-re_rate  = ls_prov-re_rate  / lv_factor.
   MODIFY gt_prov FROM ls_prov INDEX gv_tabix.
 ENDLOOP.

 LOOP AT gt_prov INTO ls_prov.
   gv_tabix  = sy-tabix.
   gv_tabix2 = gv_tabix - 1.

   CLEAR: ls_prov2.
   READ TABLE gt_prov INTO ls_prov2 INDEX gv_tabix2.

   ls_prov-mo_stock = ( ls_prov2-mo_stock + ls_prov-mo_return ) - ls_dprov-mo_oxide.

   MODIFY gt_prov FROM ls_prov INDEX gv_tabix.
 ENDLOOP.

ENDFORM.
***==================================================================***
*                  Module  STATUS_9001  OUTPUT
***==================================================================***
*                  Crea container , catalogo y muestra alv
***==================================================================***
MODULE status_9000 OUTPUT.

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR  'ZZTITLE'.
  main_tab-activetab = i_main_tab-pressed_tab.

  CASE i_main_tab-pressed_tab.
   WHEN c_main_tab-tab1.
* Crea el objeto contenedor
      PERFORM create_object_deli USING 'CC_9100' .
* Constuye el catalago
      PERFORM build_fieldcatalog_deli USING gs_fieldcat[].
      i_main_tab-subscreen = '9100'.
* Despliega los datos
      PERFORM display_output_deli USING gt_rep.

   WHEN c_main_tab-tab2.
* Crea el objeto contenedor
      PERFORM create_object_prov USING 'CC_9200' .
* Constuye el catalago
      PERFORM build_fieldcatalog_return USING gs_fieldcat[].
      i_main_tab-subscreen = '9200'.
* Despliega los datos
      PERFORM display_output_prov USING gt_prov.

   WHEN c_main_tab-tab3.
* Crea el objeto contenedor
      PERFORM create_object_fin USING 'CC_9300' .
* Constuye el catalago
      PERFORM build_fieldcatalog_return USING gs_fieldcat[].
      i_main_tab-subscreen = '9300'.
* Despliega los datos
      PERFORM display_output_fin USING gt_fin.

   WHEN c_main_tab-tab4.
* Crea el objeto contenedor
      PERFORM create_object_diff USING 'CC_9400' .
* Constuye el catalago
      PERFORM build_fieldcatalog_diff USING gs_fieldcat[].
      i_main_tab-subscreen = '9400'.
* Despliega los datos
      PERFORM display_output_diff USING gt_diff.

   WHEN c_main_tab-tab5. " Volumen

     PERFORM create_object_volu USING 'CC_9500'.

     PERFORM build_fieldcatalog_volume USING gs_fieldcat[].
     i_main_tab-subscreen = '9500'.

     PERFORM display_output_volu USING gt_volu.

  WHEN c_main_tab-tab6.
** Crea el Objeto contenedor.
     PERFORM create_object_pdeli USING 'CC_9600'.
** Constuye el catalago
     PERFORM build_fieldcatalog_penal USING gs_fieldcat[].
      i_main_tab-subscreen = '9600'.
** Despliega los datos
      PERFORM display_output_pdeli USING gt_pdeli.

  WHEN c_main_tab-tab7.

      PERFORM create_object_pprov USING 'CC_9700'.
** catalogo
      PERFORM build_fieldcatalog_penal USING gs_fieldcat[].
      i_main_tab-subscreen = '9700'.
** Despliega los datos
      PERFORM display_output_pprov USING gt_pprov.

  WHEN c_main_tab-tab8.

      PERFORM create_object_pfin USING 'CC_9800'.
** catalogo
      PERFORM build_fieldcatalog_penal USING gs_fieldcat[].
      i_main_tab-subscreen = '9800'.
** Despliega los datos
      PERFORM display_output_pfin USING gt_pfin.

  WHEN c_main_tab-tab9. " volumen final

***
      PERFORM create_object_volfin USING 'CC_9900'.
**
      PERFORM build_fieldcatalog_volume USING gs_fieldcat[].
      i_main_tab-subscreen = '9900'.
** Despliega los datos
      PERFORM display_output_volfin USING gt_volfin.

  ENDCASE.

ENDMODULE.                 " STATUS_9001  OUTPUT
***==================================================================***
*&      Module  USER_COMMAND_9000  INPUT
***==================================================================***
*       PAI
***==================================================================***
MODULE user_command_9000 INPUT.
  DATA: lv_ucomm TYPE sy-ucomm.
        lv_ucomm =    sy-ucomm.

  CASE lv_ucomm.
    WHEN 'CANCEl' OR 'EXIT'.
      PERFORM free_objects.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects.
      SET SCREEN '0'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

FORM refresh_table.
DATA: l_valid TYPE char01.

IF o_grid2 IS BOUND.
  CALL METHOD o_grid2->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_dprov IS BOUND.
  CALL METHOD o_grid_dprov->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_dfin IS BOUND.
  CALL METHOD o_grid_dfin->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_pddeli IS BOUND.
   CALL METHOD o_grid_pddeli->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_pdprov IS BOUND.
   CALL METHOD o_grid_pdprov->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_dvolfin IS BOUND.
    CALL METHOD o_grid_dvolfin->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_dvolu IS BOUND.
  CALL METHOD o_grid_dvolu->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_pddeli IS BOUND.
   CALL METHOD o_grid_pddeli->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_pdfin IS BOUND.
  CALL METHOD o_grid_pdfin->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

IF o_grid_pdprov IS  BOUND.
    CALL METHOD o_grid_pdprov->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDIF.

  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.

IF o_grid2 IS BOUND.
  CALL METHOD o_grid2->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

 IF o_grid_dprov IS BOUND.
  CALL METHOD o_grid_dprov->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

 IF o_grid_dfin IS BOUND.
  CALL METHOD o_grid_dfin->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_pddeli IS BOUND.
  CALL METHOD o_grid_pddeli->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_pdprov IS BOUND.
  CALL METHOD o_grid_pdprov->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_dvolfin IS BOUND.
  CALL METHOD o_grid_dvolfin->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_dvolu IS BOUND.
  CALL METHOD o_grid_dvolu->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_pddeli IS BOUND.
  CALL METHOD o_grid_pddeli->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_pdfin IS BOUND.
  CALL METHOD o_grid_pdfin->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

  IF o_grid_pdprov IS BOUND.
  CALL METHOD o_grid_pdprov->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
 ENDIF.

ENDFORM.
***==================================================================***
*                  Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
***==================================================================***
*                  This is used to catch the pressed tab
***==================================================================***
MODULE main_tab_active_tab_get INPUT.
  CASE sy-ucomm.
  WHEN c_main_tab-tab1.
       i_main_tab-pressed_tab = c_main_tab-tab1. " Delivery
       i_main_tab-subscreen = '9100'.

  WHEN c_main_tab-tab2.
       i_main_tab-pressed_tab = c_main_tab-tab2. " Return ( provisional)
       i_main_tab-subscreen = '9200'.

  WHEN c_main_tab-tab3.
       i_main_tab-pressed_tab = c_main_tab-tab3. " Return ( final )
       i_main_tab-subscreen = '9300'.

  WHEN c_main_tab-tab4.
       i_main_tab-pressed_tab = c_main_tab-tab4. " Difference (Provisional-Final)
       i_main_tab-subscreen = '9400'.

  WHEN c_main_tab-tab5.
       i_main_tab-pressed_tab = c_main_tab-tab5. " Maximum Volume for Low Quality (kg)
       i_main_tab-subscreen = '9500'.

  WHEN c_main_tab-tab6.
       i_main_tab-pressed_tab = c_main_tab-tab6. " penalites delivery
       i_main_tab-subscreen = '9600'.

  WHEN c_main_tab-tab7.
       i_main_tab-pressed_tab = c_main_tab-tab7. " penalties provisional
       i_main_tab-subscreen = '9700'.

  WHEN c_main_tab-tab8.
       i_main_tab-pressed_tab = c_main_tab-tab8. " penalties final
       i_main_tab-subscreen = '9800'.

  WHEN c_main_tab-tab9.
       i_main_tab-pressed_tab = c_main_tab-tab9. " volumen
       i_main_tab-subscreen = '9900'.


    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.                 " MAIN_TAB_ACTIVE_TAB_GET  INPUT
***==================================================================***
*&      Form  create_object
***==================================================================***
*       Creating Docking Container and grid
***==================================================================***
FORM create_object_deli USING custom.

IF o_custom IS INITIAL.
* Creating Docking Container
  CREATE OBJECT o_custom
         EXPORTING
         container_name = custom
         EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

   CREATE OBJECT ob_split1
      EXPORTING
        parent      = o_custom
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid
        EXPORTING
           i_parent          = ob_split1->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object

***==================================================================***
*&      Form  create_object o_custom_pdeli
***==================================================================***
*       Creating Docking Container and grid
***==================================================================***
FORM create_object_pdeli USING custom.

IF o_custom_pdeli IS INITIAL.
* Creating Docking Container
  CREATE OBJECT o_custom_pdeli
         EXPORTING
         container_name = custom
         EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

   CREATE OBJECT ob_split_pdeli
      EXPORTING
        parent      = o_custom_pdeli
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid_pdeli
        EXPORTING
           i_parent          = ob_split_pdeli->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object
***==================================================================***
*                  Form  create_object_pddeli
***==================================================================***
*                  Objetivo: Desplegar el segundo ALV con los detalles
***==================================================================***
FORM create_object_pddeli USING custom.

 IF o_grid_pddeli IS BOUND.
    o_grid_pddeli->free( ).
    CLEAR o_grid_pddeli.
 ENDIF.

  CREATE OBJECT o_grid_pddeli
    EXPORTING
      i_parent = ob_split_pdeli->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.                    " create_object_pdeli
***==================================================================***
*&      Form  create_object o_custom_pprov
***==================================================================***
*       Creating Docking Container and grid
***==================================================================***
FORM create_object_pprov USING custom.

IF o_custom_pprov IS INITIAL.
* Creating Docking Container
  CREATE OBJECT o_custom_pprov
         EXPORTING
         container_name = custom
         EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

   CREATE OBJECT ob_split_pprov
      EXPORTING
        parent      = o_custom_pprov
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid_pprov
        EXPORTING
           i_parent          = ob_split_pprov->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object
***==================================================================***
*                  Form  create_object_pdprov
***==================================================================***
*                  Objetivo: Desplegar el segundo ALV con los detalles
***==================================================================***
FORM create_object_pdprov USING custom.

 IF o_grid_pdprov IS BOUND.
    o_grid_pdprov->free( ).
    CLEAR o_grid_pdprov.
 ENDIF.

  CREATE OBJECT o_grid_pdprov
    EXPORTING
      i_parent = ob_split_pprov->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.                    " create_object_pprov
***==================================================================***
*&      Form  create_object o_custom_pfin
***==================================================================***
*       Creating Docking Container and grid
***==================================================================***
FORM create_object_pfin USING custom.

IF o_custom_pfin IS INITIAL.
* Creating Docking Container
  CREATE OBJECT o_custom_pfin
         EXPORTING
         container_name = custom
         EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

   CREATE OBJECT ob_split_pfin
      EXPORTING
        parent      = o_custom_pfin
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid_pfin
        EXPORTING
           i_parent          = ob_split_pfin->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object_pfin
***==================================================================***
*                  Form  create_object_pdfin
***==================================================================***
*                  Objetivo: Desplegar el segundo ALV con los detalles
***==================================================================***
FORM create_object_pdfin USING custom.

 IF o_grid_pdfin IS BOUND.
    o_grid_pdfin->free( ).
    CLEAR o_grid_pdfin.
 ENDIF.

  CREATE OBJECT o_grid_pdfin
    EXPORTING
      i_parent = ob_split_pfin->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.                    " create_object_pdfin
***==================================================================***
*                  Form  create_object_prov
***==================================================================***
*
***==================================================================***
FORM create_object_prov USING custom.

IF o_custom_prov IS INITIAL.
* Creating Docking Container
  CREATE OBJECT o_custom_prov
      EXPORTING
         container_name = custom
     EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT ob_split_prov
      EXPORTING
        parent      = o_custom_prov
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid_prov
        EXPORTING
           i_parent          = ob_split_prov->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object

FORM create_object_diff USING custom.

IF o_custom_diff IS INITIAL.

* Creating Docking Container
  CREATE OBJECT o_custom_diff
      EXPORTING
         container_name = custom
     EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc EQ 0.
* Creating Grid

   CREATE OBJECT o_grid_diff
        EXPORTING
           i_parent      = o_custom_diff.
      "     i_parent          = ob_split_diff->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object_diff

FORM create_object_volu USING custom.

IF o_custom_volu IS INITIAL.

* Creating Docking Container
  CREATE OBJECT o_custom_volu
      EXPORTING
         container_name = custom
     EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.


  CREATE OBJECT ob_split_volu
      EXPORTING
        parent      = o_custom_volu
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
   CREATE OBJECT o_grid_volu
        EXPORTING
           i_parent =  ob_split_volu->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object_volu

FORM create_object_volfin USING custom.

IF o_custom_volfin IS INITIAL.

* Creating Docking Container
  CREATE OBJECT o_custom_volfin
      EXPORTING
         container_name = custom
     EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.


  CREATE OBJECT ob_split_volfin
      EXPORTING
        parent      = o_custom_volfin
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
   CREATE OBJECT o_grid_volfin
        EXPORTING
           i_parent =  ob_split_volfin->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object_diff
*&---------------------------------------------------------------------*
*&      Form  create_object_dfin
*&---------------------------------------------------------------------*
*       Crear   o_grid_dfin
*----------------------------------------------------------------------*
FORM create_object_dvolu USING custom.

 IF o_grid_dvolu IS BOUND.
    o_grid_dvolu->free( ).
    CLEAR o_grid_dvolu.
 ENDIF.

  CREATE OBJECT o_grid_dvolu
    EXPORTING
      i_parent = ob_split_volu->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_object_dvolfin
*&---------------------------------------------------------------------*
*       Crear  Grid alv o_grid_dvolfin
*----------------------------------------------------------------------*
FORM create_object_dvolfin USING custom.

 IF o_grid_dvolfin IS BOUND.
    o_grid_dvolfin->free( ).
    CLEAR o_grid_dvolfin.
 ENDIF.

  CREATE OBJECT o_grid_dvolfin
    EXPORTING
      i_parent = ob_split_volfin->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.
***==================================================================***
*                  Form  BUILD_FIELDCATALOG2
***==================================================================***
*                  Objetivo: Desplegar el segundo ALV con los detalles
***==================================================================***
FORM create_object_ddeli USING custom.

 IF o_grid2 IS BOUND.
    o_grid2->free( ).
    CLEAR o_grid2.
 ENDIF.

 IF o_grid_dprov IS BOUND.
    o_grid_dprov->refresh_table_display( ).
    o_grid_dprov->free( ).
    CLEAR o_grid_dprov.
 ENDIF.

  CREATE OBJECT o_grid2
    EXPORTING
      i_parent = ob_split1->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.                    " create_object2

FORM create_object_dprov USING custom.

 IF o_grid_dprov IS BOUND.
    o_grid_dprov->free( ).
    CLEAR o_grid_dprov.
 ENDIF.

  CREATE OBJECT o_grid_dprov
    EXPORTING
      i_parent = ob_split_prov->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.


FORM create_object_fin USING custom.

IF o_custom_fin IS INITIAL.
* Creating Docking Container
      CREATE OBJECT o_custom_fin
         EXPORTING
         container_name = custom
         EXCEPTIONS
         cntl_error                  = 1
         cntl_system_error           = 2
         create_error                = 3
         lifetime_error              = 4
         lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT ob_split_fin
      EXPORTING
        parent      = o_custom_fin
        orientation = cl_gui_easy_splitter_container=>orientation_vertical.

  IF sy-subrc EQ 0.
* Creating Grid
    CREATE OBJECT o_grid_fin
        EXPORTING
           i_parent          = ob_split_fin->top_left_container.
  ENDIF.

  ENDIF.
ENDFORM.                    " create_object
*&---------------------------------------------------------------------*
*&      Form  create_object_dfin
*&---------------------------------------------------------------------*
*       Crear   o_grid_dfin
*----------------------------------------------------------------------*
FORM create_object_dfin USING custom.

 IF o_grid_dfin IS BOUND.
    o_grid_dfin->free( ).
    CLEAR o_grid_dfin.
 ENDIF.

  CREATE OBJECT o_grid_dfin
    EXPORTING
      i_parent = ob_split_fin->bottom_right_container. "Funcion usada para que dimenciones de alv sean modificable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_fieldcat
*&---------------------------------------------------------------------*
*       Filling the fieldcatalog table
*----------------------------------------------------------------------*
*FORM create_fieldcat USING value(p_structure).
** Clearing the contents of the fieldcatalog
*  REFRESH i_fieldcat.
** Filling the fieldcatalog table
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name       = p_structure
*    CHANGING
*      ct_fieldcat            = i_fieldcat
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*ENDFORM.                    " create_fieldcat
*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       Displaying the output
*----------------------------------------------------------------------*
FORM display_output_deli USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.


  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid.
ENDFORM.                    " display_output
***==================================================================***
**                 FORM display_output_pdeli
***==================================================================***
**                 Prepara salida ALV Penalties Mensual delivery
***==================================================================***
FORM display_output_pdeli USING itab
                                     .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pdeli->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_pdeli.

ENDFORM.                " display_output_pdeli
***==================================================================***
**                 FORM display_output_pddeli
***==================================================================***
**                 Prepara salida ALV Penalties Diario delivery
***==================================================================***
FORM display_output_pddeli USING itab .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pddeli->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_pddeli.

ENDFORM.                " display_output_pddeli
***==================================================================***
**                 FORM display_output_pprov
***==================================================================***
**                 Prepara salida ALV Penalties Mensual provisional
***==================================================================***
FORM display_output_pprov USING itab
                                     .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pprov->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_pprov.

ENDFORM.                " display_output_pdeli
***==================================================================***
**                 FORM display_output_pdprov
***==================================================================***
**                 Prepara salida ALV Penalties Mensual provisional
***==================================================================***
FORM display_output_pdprov USING itab
                                     .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pdprov->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_pdprov.

ENDFORM.                " display_output_pdprov
***==================================================================***
**                 FORM display_output_prov USING
***==================================================================***
**                 Prepara salida ALV
***==================================================================***
FORM display_output_prov USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_prov->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_prov.
ENDFORM.                    " display_output
***==================================================================***
*                  Form  display_output_fin
***==================================================================***
*                  Displaying the output
***==================================================================***
FORM display_output_fin USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.
    w_variant-report = sy-repid.

* Displaying the output
  CALL METHOD o_grid_fin->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_fin.
ENDFORM.                    " display_output
***==================================================================***
**                 FORM display_output_pfin
***==================================================================***
**                 Prepara salida ALV Penalties Mensual provisional
***==================================================================***
FORM display_output_pfin USING itab
                                     .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pfin->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
  CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click FOR o_grid_pfin.

ENDFORM.                " display_output_pfin
***==================================================================***
**                 FORM display_output_pfin
***==================================================================***
**                 Prepara salida ALV Penalties Mensual provisional
***==================================================================***
FORM display_output_pdfin USING itab .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_pdfin->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
  CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click FOR o_grid_pdfin.

ENDFORM.                " display_output_pdfin
***==================================================================***
*                  FORM display_output_diff
***==================================================================***
*                  Prepara salida ALV Diferencia prov-fin
***==================================================================***
FORM display_output_diff USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.


  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_diff->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_diff.
ENDFORM.                    " display_output
***==================================================================***
*                  FORM display_output_volu
***==================================================================***
*                  Prepara salida ALV Volumen
***==================================================================***
FORM display_output_volu USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

    w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_volu->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_volu.
ENDFORM.                    " display_output
***==================================================================***
*                  FORM display_output_dvolu
***==================================================================***
*                  Prepara salida ALV diario Volumen delivery
***==================================================================***
FORM display_output_dvolu USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.


  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_dvolu->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_dvolu.
ENDFORM.                    " display_output
***==================================================================***
*                  FORM display_output_volfin
***==================================================================***
*                  Prepara salida ALV Volumen final
***==================================================================***
FORM display_output_volfin USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

  w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_volfin->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_volfin.
ENDFORM.                    " display_output
***==================================================================***
*                  FORM display_output_volu
***==================================================================***
*                  Prepara salida ALV Volumen
***==================================================================***
FORM display_output_dvolfin USING itab .

* Define Layout
    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

    w_variant-report = sy-repid.
* Displaying the output
  CALL METHOD o_grid_dvolfin->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_dvolfin.
ENDFORM.                    " display_output
***==================================================================***
**                        FORM display_output_ddeli
***==================================================================***
**                        Prepara salida ALV Diario delivery
***==================================================================***
FORM display_output_ddeli USING itab .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid2->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      "i_consistency_check          = 'X'
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid2.

ENDFORM.                " display_output_ddeli
***==================================================================***
**                        From display_output_dprov
***==================================================================***
**                        Crear catalogo diario provisional
***==================================================================***
FORM display_output_dprov USING itab .
  w_variant-report = sy-repid.

   gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_dprov->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_dprov.

ENDFORM.                    " display_output
***==================================================================***
**                        From display_output_dfin
***==================================================================***
**                        Prepara salida ALV Diario Fin
***==================================================================***
FORM display_output_dfin USING itab .
  w_variant-report = sy-repid.

    gs_layout-col_opt           = 'X'.
    gs_layout-zebra             = 'X'.
    gs_layout-box_fname         = space.
    gs_layout-sel_mode          = 'D'.
    x_save                      = 'A'.
    gs_layout-stylefname        = space.

* Displaying the output
  CALL METHOD o_grid_dfin->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      is_variant                    = w_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = gs_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Se crean los eventos de alv
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click  FOR o_grid_dfin.

ENDFORM.                    " display_output_dfin
***==================================================================***
**                        FORM free_objects
***==================================================================***
**                        Libera objetos de la clase  alv_grid
***==================================================================***
FORM free_objects.

*** Free objects Delivery
IF o_grid IS BOUND.
  CALL METHOD o_grid->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

IF o_grid2 IS BOUND.
    CALL METHOD o_grid2->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

IF o_custom IS BOUND.
  CALL METHOD o_custom->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

IF o_custom_prov IS BOUND.
*** Free Object Provisional
    CALL METHOD o_custom_prov->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

IF o_grid_prov IS BOUND.
    CALL METHOD o_grid_prov->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

IF o_grid_dprov IS BOUND.
    CALL METHOD o_grid_dprov->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDIF.

ENDFORM.                    " free_objects
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click USING  p_e_row
                                 p_e_column
                                 p_es_row_no TYPE lvc_s_roid.
  CASE p_e_column.
    WHEN 'MONTH'.
* Despliega segun pestaña
 CASE i_main_tab-pressed_tab.
 	WHEN 'MAIN_TAB_TAB1'.
      PERFORM refresh_table.
* Creating Object
      PERFORM create_object_ddeli USING 'CC_9100'.
* Building the field catalog
      PERFORM build_fieldcatalog_ddeli USING gs_fieldcat[].
      i_main_tab-subscreen = '9100'.

      CLEAR: ls_rep.
      READ TABLE gt_rep INTO ls_rep INDEX p_es_row_no-row_id.
      REFRESH: aux_ddeli.

      aux_ddeli[] = gt_ddeli[].
* Elimino los registros que no corresponden al mes.
      DELETE aux_ddeli WHERE dias+0(6) <> ls_rep-month.
* Displaying data Diario delivery
      PERFORM display_output_ddeli USING aux_ddeli.

 	WHEN 'MAIN_TAB_TAB2'.
      PERFORM refresh_table.
* Crea el objecto contendor
      PERFORM create_object_dprov USING 'CC_9200'.
* Crea el catalogo
      PERFORM build_fieldcatalog_dreturn USING gs_fieldcat[].
       i_main_tab-subscreen = '9200'.

      CLEAR: ls_prov.
      READ TABLE gt_prov INTO ls_prov INDEX p_es_row_no-row_id.
      REFRESH: aux_dprov.

      aux_dprov[] = gt_dprov[].
* Dejamos solo los registros del mes
      DELETE aux_dprov WHERE dias+0(6) <> ls_prov-month.
* Depliega el alv
      PERFORM display_output_dprov USING aux_dprov.

 WHEN 'MAIN_TAB_TAB3'.
      PERFORM refresh_table.
*   * Crea el objecto contendor
      PERFORM create_object_dfin USING 'CC_9300'.
** Crea el catalogo
      REFRESH gs_fieldcat.
      PERFORM build_fieldcatalog_dreturn USING gs_fieldcat[].
       i_main_tab-subscreen = '9300'.
*
      CLEAR: ls_fin.
      READ TABLE gt_fin INTO ls_fin INDEX p_es_row_no-row_id.
      REFRESH: aux_dfin .

      aux_dfin[] = gt_dfin[].
* Dejamos solo los registros del mes
      DELETE aux_dfin WHERE dias+0(6) <> ls_fin-month.
* Depliega el alv
      PERFORM display_output_dfin USING aux_dfin.

  WHEN 'MAIN_TAB_TAB5'.
      PERFORM refresh_table.

      PERFORM create_object_dvolu USING 'CC_9500'.

      PERFORM build_fieldcatalog_dvolume USING gs_fieldcat[].
      i_main_tab-subscreen = '9500'.

      CLEAR: ls_volu.
      READ TABLE gt_volu INTO ls_volu INDEX p_es_row_no-row_id.
      aux_dvolu[] = gt_dvolu.

      DELETE aux_dvolu WHERE dias+0(6) <> ls_volu-month.

      PERFORM display_output_dvolu USING aux_dvolu.

  WHEN 'MAIN_TAB_TAB6'.

      PERFORM refresh_table.
*    Crea el objecto contendor
      PERFORM create_object_pddeli USING 'CC_9600'.

      PERFORM build_fieldcatalog_dpenal USING gs_fieldcat[].
       i_main_tab-subscreen = '9600'.
*
      CLEAR: ls_pdeli.
      READ TABLE gt_pdeli INTO ls_pdeli INDEX p_es_row_no-row_id.
      REFRESH: aux_pddeli .
*
      aux_pddeli[] = gt_pddeli[].

      DELETE aux_pddeli WHERE dias+0(6) <> ls_pdeli-month.
*
      PERFORM display_output_pddeli USING aux_pddeli.

 WHEN 'MAIN_TAB_TAB7'.

      PERFORM refresh_table.

      PERFORM create_object_pdprov USING 'CC_9700'.

      PERFORM build_fieldcatalog_dpenal USING gs_fieldcat[].
      i_main_tab-subscreen = '9700'.

      CLEAR: ls_pprov.
      READ TABLE gt_pprov INTO ls_pprov INDEX p_es_row_no-row_id.
      REFRESH aux_pdprov.

      aux_pdprov[] = gt_pdprov.
      DELETE aux_pdprov WHERE dias+0(6) <> ls_pprov-month.

      PERFORM display_output_pdprov USING aux_pdprov.

  WHEN 'MAIN_TAB_TAB8'.

      PERFORM refresh_table.

      PERFORM create_object_pdfin USING 'CC_9800'.

      PERFORM build_fieldcatalog_dpenal USING gs_fieldcat[].
      i_main_tab-subscreen = '9800'.

      CLEAR: ls_pfin.
      READ TABLE gt_pfin INTO ls_pfin INDEX p_es_row_no-row_id.
      REFRESH aux_pdfin.

      aux_pdfin[] = gt_pdfin.
      DELETE aux_pdfin WHERE dias+0(6) <> ls_pfin-month.

      PERFORM display_output_pdfin USING aux_pdfin.

 WHEN 'MAIN_TAB_TAB9'.

      PERFORM refresh_table.

      PERFORM create_object_dvolfin USING 'CC_9900'.

      PERFORM build_fieldcatalog_dvolume USING gs_fieldcat[].
      i_main_tab-subscreen = '9900'.

      CLEAR: ls_volfin.
      READ TABLE gt_volfin INTO ls_volfin INDEX p_es_row_no-row_id.
      aux_dvolfin[] = gt_dvolfin.

      DELETE aux_dvolfin WHERE dias+0(6) <> ls_volfin-month.

      PERFORM display_output_dvolfin USING aux_dvolfin.

 	WHEN OTHERS.
 ENDCASE.

      WHEN 'VBELN'.
CLEAR: lv_vbeln.

CASE i_main_tab-pressed_tab..
  WHEN 'MAIN_TAB_TAB1'.

      CLEAR: ls_ddeli.
      READ TABLE aux_ddeli INTO ls_ddeli INDEX p_es_row_no-row_id.
      lv_vbeln = ls_ddeli-vbeln.

  WHEN 'MAIN_TAB_TAB2'.

      CLEAR: ls_dprov.
      READ TABLE aux_dprov INTO ls_dprov INDEX p_es_row_no-row_id.
      lv_vbeln = ls_dprov-vbeln.
  WHEN 'MAIN_TAB_TAB3'.

      CLEAR: ls_dprov.
      READ TABLE aux_dfin INTO ls_dfin INDEX p_es_row_no-row_id.
      lv_vbeln = ls_dfin-vbeln.

  WHEN 'MAIN_TAB_TAB4'.

  WHEN 'MAIN_TAB_TAB5'.

      CLEAR: ls_dvolu.
      READ TABLE aux_dvolu INTO ls_dvolu INDEX p_es_row_no-row_id.
      lv_vbeln = ls_dvolu-vbeln.

  WHEN 'MAIN_TAB_TAB6'.

      CLEAR: ls_pddeli.
      READ TABLE aux_pddeli INTO ls_pddeli INDEX p_es_row_no-row_id.
      lv_vbeln = ls_pddeli-vbeln.

  WHEN 'MAIN_TAB_TAB7'.

      CLEAR: ls_pdprov.
      READ TABLE aux_pdprov INTO ls_pdprov INDEX p_es_row_no-row_id.
      lv_vbeln = ls_pdprov-vbeln.

  WHEN 'MAIN_TAB_TAB8'.

      CLEAR: ls_pdfin.
      READ TABLE aux_pdfin INTO ls_pdfin INDEX p_es_row_no-row_id.
      lv_vbeln = ls_pdfin-vbeln.


  WHEN 'MAIN_TAB_TAB9'.

      CLEAR: ls_dvolfin.
      READ TABLE aux_dvolfin INTO ls_dvolfin INDEX p_es_row_no-row_id.
      lv_vbeln = ls_dvolfin-vbeln.

  WHEN OTHERS.
ENDCASE.

       IF lv_vbeln IS NOT INITIAL.
        SET PARAMETER ID 'VL' FIELD lv_vbeln.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
       ENDIF.

      WHEN 'CHARG'.
      CLEAR: lv_mantr , lv_charg.
CASE i_main_tab-pressed_tab..
  WHEN 'MAIN_TAB_TAB1'.

      CLEAR: ls_ddeli.
      READ TABLE aux_ddeli INTO ls_ddeli INDEX p_es_row_no-row_id.
      lv_mantr = ls_ddeli-matnr.
      lv_charg = ls_ddeli-charg.

  WHEN 'MAIN_TAB_TAB2'.

      CLEAR: ls_dprov.
      READ TABLE aux_dprov INTO ls_dprov INDEX p_es_row_no-row_id.
      lv_mantr = ls_dprov-matnr.
      lv_charg = ls_dprov-charg.

  WHEN 'MAIN_TAB_TAB3'.

      CLEAR: ls_dprov.
      READ TABLE aux_dfin INTO ls_dfin INDEX p_es_row_no-row_id.
      lv_mantr = ls_dfin-matnr.
      lv_charg = ls_dfin-charg.

  WHEN 'MAIN_TAB_TAB4'.

  WHEN 'MAIN_TAB_TAB5'.

      CLEAR: ls_dvolu.
      READ TABLE aux_dvolu INTO ls_dvolu INDEX p_es_row_no-row_id.
      lv_mantr = ls_dvolu-matnr.
      lv_charg = ls_dvolu-charg.


  WHEN 'MAIN_TAB_TAB6'.

      CLEAR: ls_pddeli.
      READ TABLE aux_pddeli INTO ls_pddeli INDEX p_es_row_no-row_id.
      lv_mantr = ls_pddeli-matnr.
      lv_charg = ls_pddeli-charg.

  WHEN 'MAIN_TAB_TAB7'.

      CLEAR: ls_pdprov.
      READ TABLE aux_pdprov INTO ls_pdprov INDEX p_es_row_no-row_id.
      lv_mantr = ls_pdprov-matnr.
      lv_charg = ls_pdprov-charg.

  WHEN 'MAIN_TAB_TAB8'.

      CLEAR: ls_pdfin.
      READ TABLE aux_pdfin INTO ls_pdfin INDEX p_es_row_no-row_id.
      lv_mantr = ls_pdfin-matnr.
      lv_charg = ls_pdfin-charg.

  WHEN 'MAIN_TAB_TAB9'.

      CLEAR: ls_dvolfin.
      READ TABLE aux_dvolfin INTO ls_dvolfin INDEX p_es_row_no-row_id.
      lv_mantr = ls_dvolfin-matnr.
      lv_charg = ls_dvolfin-charg.

  WHEN OTHERS.
ENDCASE.

     IF lv_mantr IS NOT INITIAL AND lv_charg IS NOT INITIAL.
        SET PARAMETER ID 'MAT' FIELD lv_mantr.
        SET PARAMETER ID 'CHA' FIELD lv_charg.
        CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.
      ENDIF.

ENDCASE.

ENDFORM.
***==================================================================***
*                  FORM load_ddeli.
***==================================================================***
*                  obj.Cargar Tabla interna del Diario Delivery
***==================================================================***
FORM load_ddeli.

  LOOP AT gt_lips ASSIGNING <lx_lips>.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = <lx_lips>-vbeln BINARY SEARCH.

   ls_ddeli-dias  = ls_likp-wadat_ist.
   ls_ddeli-vbeln = ls_likp-vbeln.
   ls_ddeli-charg = <lx_lips>-charg.
   ls_ddeli-matnr = <lx_lips>-matnr.

      CLEAR:  lv_bag,        " Numero de Bags (MaxiSacos)
              lv_gross,      " Gross Weight (KG)
              lv_tare,       " Tare (KG)
              lv_net,        " Net Weight (KG)
              lv_moisture,   " Moisture (%)
              lv_dry.        " Dry Weight (KG)

          CALL FUNCTION 'ZQM012_PESOS'
            EXPORTING
              i_entrega = <lx_lips>-vbeln
              i_matnr   = <lx_lips>-matnr
              i_lote    = <lx_lips>-charg
              i_bwart   = '641'
            IMPORTING
              gross     = lv_gross
              bag       = lv_bag
              tare      = lv_tare
              moisture  = lv_moisture
              net       = lv_net
              dry       = lv_dry.

       MOVE lv_gross    TO ls_ddeli-gross.  " Gross Weight (KG)
       MOVE lv_bag      TO ls_ddeli-bag.    " Numero de bultos
       MOVE lv_tare     TO ls_ddeli-tara.   " Tare (KG)
       MOVE lv_net      TO ls_ddeli-net.    " Net Weight (KG
       MOVE lv_moisture TO ls_ddeli-humedad." Humedad(%)
       MOVE lv_dry      TO ls_ddeli-dry.    " Dry Weight (KG)

CLEAR: lv_cuobj_bm,lv_ca,lv_sodio,lv_arsenico.
SELECT SINGLE cuobj_bm INTO lv_cuobj_bm
         FROM mch1
        WHERE matnr EQ <lx_lips>-matnr AND
              charg EQ <lx_lips>-charg.

      REFRESH: lt_ausp[].
      SELECT a~atinn a~atwrt a~atflv  a~atflb c~atnam c~atfor
  INTO TABLE lt_ausp
        FROM ausp AS a INNER JOIN
             cabn AS c ON c~atinn EQ a~atinn
       WHERE a~objek EQ lv_cuobj_bm AND
             c~atfor EQ 'NUM'.

 IF sy-subrc EQ 0.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'LEY_MO'.
   IF sy-subrc EQ 0.
   ls_ddeli-ley_mo = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'Z01_CA'.
   IF sy-subrc EQ 0.
   lv_ca = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'Z01_NA2O'.
   IF sy-subrc EQ 0.
   lv_sodio = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'Z01_AS'.
   IF sy-subrc EQ 0.
   lv_arsenico = ls_ausp-atflv.
   ENDIF.
   ENDIF.

* CLEAR: lv_objectkey.
* CONCATENATE <lx_lips>-matnr <lx_lips>-charg INTO lv_objectkey.

* Obtengo las caracteristicas de lote
* CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*     EXPORTING
*       objectkey              = lv_objectkey
*       objecttable            = 'MCH1'
*       classnum               = 'CONCENTRADO_MO'
*       classtype              = '023'
*       keydate                = sy-datum
*       language               = sy-langu
*     TABLES
*       allocvaluesnum         = t_num
*       allocvalueschar        = t_char
*       allocvaluescurr        = t_curr
*       return                 = t_return
*             .

** Ley Mo
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'LEY_MO'.
*  IF sy-subrc EQ 0.
*      ls_ddeli-ley_mo = ls_num-value_from.
*  ENDIF.
*
*** CA ( solo para un calculo de volumen )
*   CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'Z01_CA'.
*  IF sy-subrc EQ 0.
*      lv_ca = ls_num-value_from.
*  ENDIF.
*
****
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'Z01_NA2O'.
*  IF sy-subrc EQ 0.
*      lv_sodio = ls_num-value_from.
*  ENDIF.
*
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'Z01_AS'.
*  IF sy-subrc EQ 0.
*      lv_arsenico = ls_num-value_from.
*  ENDIF.

* recovery rate
  CLEAR: lv_ebeln.
*~~> Se Obtiene Plan de Entregas y contrato Asociado.
  SELECT SINGLE ebeln INTO lv_ebeln FROM ekbe
          WHERE belnr EQ <lx_lips>-vbeln.

  CLEAR: lv_ihrez.
  SELECT SINGLE ihrez INTO lv_ihrez FROM ekko
          WHERE ebeln EQ  lv_ebeln.

  CLEAR: ls_molib_002.
  SELECT SINGLE * INTO  ls_molib_002 FROM zmm_molib_002
          WHERE contnr EQ lv_ihrez AND
                plane  EQ lv_ebeln.

  CLEAR: lv_mo_kg, lv_mo_lb.
*** Mo content (kg)
  CLEAR: lv_cantidad.
  lv_cantidad = ls_ddeli-dry.

  CALL FUNCTION 'ZSD_CALCULO_MO_FINO_V2'
    EXPORTING
      material       = <lx_lips>-matnr
      lote           = <lx_lips>-charg
      cantidad       = lv_cantidad
      um             = 'KG'
      caract_finales = ''
    IMPORTING
      mo_fino_kg     = lv_mo_kg
      mo_fino_lb     = lv_mo_lb.

* Mo content(kg).(Dry weight * Ley MO)
    ls_ddeli-mo_content    = lv_mo_kg.

   CLEAR:lv_penal , lv_dias_retorno.
   CALL FUNCTION 'ZQM013_PENALIDADES'
     EXPORTING
       i_lote        = <lx_lips>-charg
       i_matnr       = <lx_lips>-matnr
    IMPORTING
      p_total          = lv_penal
      p_t_dias_retorno = lv_dias_retorno.

   IF ls_molib_002 IS NOT INITIAL.
*** Tasa de recuperación
    ls_ddeli-re_rate  = ls_molib_002-rrate." valor fijo

*** Processing Fee (US$/lb Mo)
   ls_ddeli-pro_fee   = ls_molib_002-pefee + lv_penal.
   ENDIF.

*** Mo returnable (kg) (Mo Content (kg) * recovery rate)
    ls_ddeli-mo_return =  ls_ddeli-mo_content * ls_ddeli-re_rate.

*** Processing Fee (US$)
    ls_ddeli-pro_fee_us  = ( ls_ddeli-mo_return * lv_lb ) * ls_ddeli-pro_fee.
   "* ls_ddeli-pro_fee.

*** Mo grade ( Mo content (kg) / Dry weight)
   IF ls_ddeli-dry <> 0.
   ls_ddeli-mo_grade = ls_ddeli-mo_content / ls_ddeli-dry.
   ENDIF.

*** Average_return
   ls_ddeli-ave_return = ls_ddeli-mo_content * ( ls_molib_002-peret + lv_dias_retorno ).

*** Calculos de volumen

* 48% =< Mo < 50%
* =SI( MO % >= 0,5 ; 0 ; SI( MO >= 48% ; MO Content;))
   IF ls_ddeli-ley_mo >= c_50.
      ls_ddeli-cal1 = 0.
   ELSE.
      IF ls_ddeli-ley_mo >= c_48.
         ls_ddeli-cal1 = ls_ddeli-mo_content.
      ENDIF.
   ENDIF.

* 47% =< Mo < 48%
* =SI(mo<0,47;0;SI(mo<0,48;mo_content;))
   IF ls_ddeli-ley_mo < c_47.
      ls_ddeli-cal2 = 0.
   ELSE.
      IF ls_ddeli-ley_mo < c_48.
        ls_ddeli-cal2 = ls_ddeli-mo_content.
      ENDIF.
   ENDIF.

* 1.2% =< Ca < 2.0%
* =SI(ca<0,012;0;SI(ca<0,02;AQ25;))
   IF lv_ca < c_0012.
      ls_ddeli-cal3 = 0.
   ELSE.
      IF lv_ca < c_002.
        ls_ddeli-cal3 = ls_ddeli-mo_content.
      ENDIF.
   ENDIF.

*0.2% =< Na2O < 0.4%
* (na2o<0,002;0;SI(na2o<0,004;AQ23;)) -<-
IF lv_sodio < c_0002 .
   ls_ddeli-cal4 = 0.
 ELSE.
        IF lv_sodio < c_0004 .
          ls_ddeli-cal4 = ls_ddeli-mo_content.

        ENDIF.
  ENDIF.

* 0.025% =< As < 0.05%
* SI(as<0,02;0;SI(as<0,04;AQ23;)) -<-
  IF lv_arsenico < c_002.
     ls_ddeli-cal5 = 0.
  ELSE.
         IF lv_arsenico < c_004.
            ls_ddeli-cal5 = ls_ddeli-mo_content.
         ENDIF.
  ENDIF.

**** Carga tabla Diario Delivery
   APPEND ls_ddeli TO gt_ddeli.

 ENDLOOP.

 LOOP AT gt_ddeli INTO ls_ddeli.
   gv_tabix  = sy-tabix.
   gv_tabix2 = gv_tabix - 1.

 CLEAR: ls_ddeli2.
 READ TABLE gt_ddeli INTO ls_ddeli2 INDEX gv_tabix2.

  ls_ddeli-delivery = ls_ddeli-gross.
  ls_ddeli-accumulation = ls_ddeli-delivery + ls_ddeli2-accumulation.

  MODIFY gt_ddeli FROM ls_ddeli INDEX gv_tabix.
 ENDLOOP.

* LOOP AT gt_ddeli INTO ls_ddeli.
*         gv_tabix = sy-tabix.
**
*   CLEAR: ls_mseg.
*   READ TABLE gt_mseg INTO ls_mseg WITH KEY budat_mkpf = ls_ddeli-dias.
*   IF sy-subrc EQ 0.
*      ls_ddeli-delivery = ls_mseg-menge.
*   ENDIF.
**
*   MODIFY gt_ddeli FROM ls_ddeli INDEX gv_tabix.
*
* ENDLOOP.
*
*
*  LOOP AT gt_mseg INTO ls_mseg.
*  CLEAR: ls_ddeli.
*  READ TABLE gt_ddeli INTO ls_ddeli WITH KEY dias = ls_mseg-budat_mkpf.
*  IF sy-subrc EQ 0.
*  ELSE.
*    CLEAR: ls_ddeli.
*    ls_ddeli-dias     = ls_mseg-budat_mkpf.
*    ls_ddeli-delivery = ls_mseg-menge.
*    APPEND ls_ddeli TO gt_ddeli.
*  ENDIF.
*  ENDLOOP.
ENDFORM.

***==================================================================***
*                  FORM load_fin.
***==================================================================***
*                  obj.    Cargar gt_fin
***==================================================================***
FORM load_fin.

  CLEAR: lv_date, lv_final_date.

  lv_date = s_wadat-low.
  ls_fin-month = lv_date+0(6).
  APPEND ls_fin TO gt_fin.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.

   ls_fin-month  = lv_final_date+0(6).
   lv_date       = lv_final_date.
   APPEND ls_fin TO gt_fin.

  ENDDO.

LOOP AT gt_fin INTO ls_fin.
  CLEAR: gv_tabix, lv_factor.
  gv_tabix = sy-tabix.

 LOOP AT gt_dfin INTO ls_dfin WHERE dias+0(6) EQ ls_fin-month.

   ADD ls_dfin-net        TO ls_fin-netwe.
   ADD ls_dfin-dry        TO ls_fin-drywe.
   ADD ls_dfin-mo_grade   TO ls_fin-mo_grade.
   ADD ls_dfin-mo_content TO ls_fin-mo_content.
   ADD ls_dfin-re_rate    TO ls_fin-re_rate.
   ADD ls_dfin-mo_return  TO ls_fin-mo_return.
   ADD ls_dfin-pro_fee    TO ls_fin-pro_fee.
   ADD ls_dfin-pro_fee_us TO ls_fin-pro_fee_us.
   ADD ls_dfin-ave_return TO ls_fin-ave_return.
   ADD ls_dfin-mo_oxide   TO ls_fin-mo_oxide.
   ADD 1 TO lv_factor.

 ENDLOOP.

     ls_fin-mo_grade = ls_fin-mo_grade / lv_factor.
     ls_fin-re_rate  = ls_fin-re_rate  / lv_factor.
     MODIFY gt_fin FROM ls_fin INDEX gv_tabix.
 ENDLOOP.

 LOOP AT gt_fin INTO ls_fin.
   gv_tabix  = sy-tabix.
   gv_tabix2 = gv_tabix - 1.

   CLEAR: ls_fin2.
   READ TABLE gt_fin INTO ls_fin2 INDEX gv_tabix2.

   ls_fin-mo_stock = ( ls_fin2-mo_stock + ls_fin-mo_return ) - ls_fin-mo_oxide.
   IF ls_fin-mo_stock  < 0.
    CLEAR: ls_fin-mo_stock .
   ENDIF.
   MODIFY gt_fin FROM ls_fin INDEX gv_tabix.
 ENDLOOP.

ENDFORM.
***==================================================================***
*                  FORM load_dfin
***==================================================================***
*                  obj.    Cargar gt_dfin
***==================================================================***
FORM load_dfin.

   LOOP AT gt_m101 ASSIGNING <lx_m101>.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = <lx_m101>-vbeln BINARY SEARCH.

   ls_dfin-dias  = ls_likp-wadat_ist.
   ls_dfin-vbeln = ls_likp-vbeln.
   ls_dfin-charg = <lx_m101>-charg.
   ls_dfin-matnr = <lx_m101>-matnr.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*   EXPORTING
*     input = ls_m101-matnr
*   IMPORTING
*     output = ls_dfin-matnr.

   CLEAR:   lv_bag,      " Numero de Bags (MaxiSacos)
            lv_gross,    " Gross Weight (KG)
            lv_tare,     " Tare (KG)
            lv_net,      " Net Weight (KG)
            lv_moisture, " Moisture (%)
            lv_dry.      " Dry Weight (KG)

          CALL FUNCTION 'ZQM012_PESOS'
            EXPORTING
              i_entrega = <lx_m101>-vbeln
              i_matnr   = <lx_m101>-matnr
              i_lote    = <lx_m101>-charg
              i_bwart   = '101'
            IMPORTING
              gross     = lv_gross
              bag       = lv_bag
              tare      = lv_tare
              moisture  = lv_moisture
              net       = lv_net
              dry       = lv_dry.

        ls_dfin-gross   = lv_gross.    " Gross Weight (KG)
        ls_dfin-bag     = lv_bag.      " Numero de bultos
        ls_dfin-tara    = lv_tare.     " Tare (KG)
        ls_dfin-net     = lv_net.      " Net Weight (KG
        ls_dfin-humedad = lv_moisture. " Humedad(%)
        ls_dfin-dry     = lv_dry.      " Dry Weight (KG)

*CLEAR: lv_cuobj_bm,lv_ca,lv_sodio,lv_arsenico.
*SELECT SINGLE cuobj_bm INTO lv_cuobj_bm
*         FROM mch1
*        WHERE matnr EQ <lx_m101>-matnr AND
*              charg EQ <lx_m101>-charg.

 CLEAR: lv_objectkey.
 CONCATENATE <lx_m101>-matnr <lx_m101>-charg INTO lv_objectkey.

 CLEAR: lv_cuobj.
 SELECT SINGLE cuobj INTO lv_cuobj
          FROM inob
         WHERE objek EQ lv_objectkey.

    REFRESH: lt_ausp[].
      SELECT a~atinn a~atwrt a~atflv  a~atflb c~atnam c~atfor
  INTO TABLE lt_ausp
        FROM ausp AS a INNER JOIN
             cabn AS c ON c~atinn EQ a~atinn
       WHERE a~objek EQ lv_cuobj AND
             c~atfor EQ 'NUM'.

 IF sy-subrc EQ 0.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'ZD1_LEYMO'.
   IF ls_ausp IS NOT INITIAL.
   ls_dfin-ley_mo = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'ZD1_CA'.
   IF ls_ausp IS NOT INITIAL.
   lv_ca = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'ZD1_NA2O'.
    IF ls_ausp IS NOT INITIAL.
   lv_sodio = ls_ausp-atflv.
   ENDIF.

   CLEAR: ls_ausp.
   READ TABLE lt_ausp INTO ls_ausp WITH KEY atnam = 'ZD1_AS'.
   IF ls_ausp IS NOT INITIAL.
   lv_arsenico = ls_ausp-atflv.
   ENDIF.

   ENDIF.
*
* CLEAR: lv_objectkey.
* CONCATENATE <lx_m101>-matnr <lx_m101>-charg INTO lv_objectkey.
*IF <lx_m101>-charg EQ 'L000000200'.

*ENDIF.
*
*
* REFRESH: t_num[].
** Obtengo las caracteristicas de lote
* CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*     EXPORTING
*       objectkey              = lv_objectkey
*       objecttable            = 'MCH1'
*       classnum               = 'CONCENTRADO_MO'
*       classtype              = '023'
*       keydate                = sy-datum
*       language               = sy-langu
*     TABLES
*       allocvaluesnum         = t_num
*       allocvalueschar        = t_char
*       allocvaluescurr        = t_curr
*       return                 = t_return.
* IF sy-subrc EQ 0.
*** Ley Mo
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'ZD1_LEYMO'.
*  IF sy-subrc EQ 0.
*      ls_dfin-ley_mo = ls_num-value_from.
*  ENDIF.
*
*** CA ( solo para un calculo de volumen'
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'ZD1_CA'.
*  IF sy-subrc EQ 0.
*      lv_ca = ls_num-value_from.
*  ENDIF.
*
***
* CLEAR: ls_num.
* READ TABLE t_num  INTO ls_num WITH KEY charact = 'ZD1_NA2O'.
*  IF sy-subrc EQ 0.
*      lv_sodio = ls_num-value_from.
*  ENDIF.
*
* CLEAR: ls_num.
* READ TABLE t_num INTO ls_num WITH KEY charact = 'ZD1_AS'.
*  IF sy-subrc EQ 0.
*      lv_arsenico = ls_num-value_from.
*  ENDIF.
*
* ENDIF.

* recovery rate

*~~> Se Obtiene Plan de Entregas y contrato Asociado.
  CLEAR: lv_ebeln.
  SELECT SINGLE ebeln INTO lv_ebeln FROM ekbe
          WHERE belnr EQ <lx_m101>-vbeln.

  CLEAR: lv_ihrez.
  SELECT SINGLE ihrez INTO lv_ihrez FROM ekko
          WHERE ebeln EQ  lv_ebeln.

  CLEAR: ls_molib_002.
  SELECT SINGLE * INTO ls_molib_002 FROM zmm_molib_002
          WHERE contnr EQ lv_ihrez AND
                plane  EQ lv_ebeln.

*** Mo content (kg)
  CLEAR: lv_mo_kg, lv_mo_lb, lv_cantidad.
  lv_cantidad = ls_dfin-dry.

  CALL FUNCTION 'ZSD_CALCULO_MO_FINO_V2'
    EXPORTING
      material       = <lx_m101>-matnr
      lote           = <lx_m101>-charg
      cantidad       = lv_cantidad
      um             = 'KG'
      caract_finales = ''
    IMPORTING
      mo_fino_kg     = lv_mo_kg
      mo_fino_lb     = lv_mo_lb.

* Mo content(kg).(Dry weight * Ley MO)
    ls_dfin-mo_content = lv_mo_kg.

   CLEAR:lv_penal , lv_dias_retorno.
   CALL FUNCTION 'ZQM013_PENALIDADES'
     EXPORTING
       i_lote        = <lx_m101>-charg
       i_matnr       = <lx_m101>-matnr
       i_finales       = 'X'
    IMPORTING
      p_total          = lv_penal
      p_t_dias_retorno = lv_dias_retorno.

   IF ls_molib_002 IS NOT INITIAL.
*** Tasa de recuperación
    ls_dfin-re_rate  = ls_molib_002-rrate." valor fijo

*** Processing Fee (US$/lb Mo)
   ls_dfin-pro_fee   = ls_molib_002-pefee + lv_penal.
   ENDIF.

*** Mo returnable (kg) (Mo Content (kg) * recovery rate)
   ls_dfin-mo_return =  ls_dfin-mo_content * ls_dfin-re_rate.

*** Processing Fee (US$)
   ls_dfin-pro_fee_us  = ls_dfin-mo_return * lv_lb * ls_dfin-pro_fee.

*** Mo grade ( Mo content (kg) / Dry weight)
   IF ls_dfin-dry <> 0.
   ls_dfin-mo_grade = ls_dfin-mo_content / ls_dfin-dry.

   ENDIF.

*** Average_return
   ls_dfin-ave_return = ls_dfin-mo_content * ( ls_molib_002-peret + lv_dias_retorno ).

*** Calculos de volumen
* 48% =< Mo < 50%
* =SI( MO % >= 0,5 ; 0 ; SI( MO >= 48% ; MO Content;))
   IF ls_dfin-ley_mo >= c_50.
      ls_dfin-cal1 = 0.
   ELSE.
      IF ls_dfin-ley_mo >= c_48.
         ls_dfin-cal1 = ls_dfin-mo_content.
      ENDIF.
   ENDIF.

* 47% =< Mo < 48%
* =SI(mo<0,47;0;SI(mo<0,48;mo_content;))
   IF ls_dfin-ley_mo < c_47.
      ls_dfin-cal2 = 0.
   ELSE.
      IF ls_dfin-ley_mo < c_48.
        ls_dfin-cal2 = ls_dfin-mo_content.
      ENDIF.
   ENDIF.

* 1.2% =< Ca < 2.0%
* (ca<0,012;0;SI(ca<0,02;AQ25;))
   IF lv_ca < c_0012.
      ls_dfin-cal3 = 0.
   ELSE.
      IF lv_ca < c_002.
        ls_dfin-cal3 = ls_dfin-mo_content.
      ENDIF.
   ENDIF.

* 0.2% =< Na2O < 0.4%
* SI(na2o<0,002;0;SI(na2o<0,004;AQ23;))
IF lv_sodio < c_0002.
   ls_dfin-cal4 = 0.
 ELSE.
        IF lv_sodio < c_0004 .
          ls_dfin-cal4 = ls_dfin-mo_content.
        ENDIF.
  ENDIF.

* 0.025% =< As < 0.05%
* SI(as<0,02;0;SI(as<0,04;AQ23;))
  IF lv_arsenico < c_002.
     ls_dfin-cal5 = 0.
  ELSE.
         IF lv_arsenico < c_004.
            ls_dfin-cal5 = ls_dfin-mo_content.
         ENDIF.
  ENDIF.

       APPEND ls_dfin TO gt_dfin.
  ENDLOOP.


*** MO_OXIDE
    LOOP AT gt_dfin INTO ls_dfin.
      CLEAR: gv_tabix .
      gv_tabix = sy-tabix.

      CLEAR: ls_mseg.
      READ TABLE gt_mseg INTO ls_mseg WITH KEY budat_mkpf = ls_dfin-dias.
      IF ls_mseg IS NOT INITIAL.
        ls_dfin-mo_oxide = ls_mseg-menge.
      ENDIF.

     MODIFY gt_dfin FROM ls_dfin INDEX gv_tabix.
    ENDLOOP.

* Movimiento del producto terminado
  LOOP AT gt_mseg INTO ls_mseg.

   CLEAR: ls_dfin.
   READ TABLE gt_dfin INTO ls_dfin WITH KEY dias = ls_mseg-budat_mkpf.
   IF ls_dfin IS NOT INITIAL.
        "ls_dprov-mo_oxide =
   ELSE.
    ls_dfin-dias     = ls_mseg-budat_mkpf.
    ls_dfin-matnr    = ls_mseg-matnr.
    ls_dfin-charg    = ls_mseg-charg.
    ls_dfin-mo_oxide = ls_mseg-menge.
    APPEND ls_dfin TO gt_dfin.
   ENDIF.

   SORT gt_dfin[] ASCENDING by dias.

  ENDLOOP.

*
 LOOP AT gt_dfin INTO ls_dfin.
   gv_tabix  = sy-tabix.
   gv_tabix2 = gv_tabix - 1.

  CLEAR: ls_dfin2.
  READ TABLE gt_dfin INTO ls_dfin2 INDEX gv_tabix2.

  ls_dfin-mo_stock = ( ls_dfin2-mo_stock + ls_dfin-mo_return ) - ls_dfin-mo_oxide.
  IF ls_dfin-mo_stock < 0.
    CLEAR: ls_dfin-mo_stock.
  ENDIF.

  MODIFY gt_dfin FROM ls_dfin INDEX gv_tabix.
 ENDLOOP.
ENDFORM.
***==================================================================***
*      FORM load_diff
***==================================================================***
*      obj. Cargar gt_diff.
***==================================================================***
FORM load_diff.

  CLEAR: lv_date, lv_final_date.

  lv_date = s_wadat-low.
  ls_diff-month = lv_date+0(6).
  APPEND ls_diff TO gt_diff.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.

   ls_diff-month  = lv_final_date+0(6).
   lv_date        = lv_final_date.
   APPEND ls_diff TO gt_diff.
  ENDDO.

LOOP AT gt_diff INTO ls_diff.
  gv_tabix = sy-tabix.

  CLEAR: ls_prov.
  READ TABLE gt_prov INTO ls_prov WITH KEY month = ls_diff-month.

  CLEAR: ls_fin.
  READ TABLE gt_fin INTO ls_fin WITH KEY month = ls_diff-month.

  ls_diff-netwe      = ls_prov-netwe - ls_fin-netwe.
  IF ls_diff-netwe < 0.
     ls_diff-netwe = ls_diff-netwe * -1.
  ENDIF.

  ls_diff-drywe      = ls_prov-drywe      - ls_fin-drywe.
  ls_diff-mo_grade   = ls_prov-mo_grade   - ls_fin-mo_grade.
  ls_diff-mo_content = ls_prov-mo_content - ls_fin-mo_content.
  ls_diff-re_rate    = ls_prov-re_rate    - ls_fin-re_rate.
  ls_diff-mo_return  = ls_prov-mo_return  - ls_fin-mo_return.
  ls_diff-pro_fee    = ls_prov-pro_fee    - ls_fin-pro_fee.
  ls_diff-pro_fee_us = ls_prov-pro_fee_us - ls_fin-pro_fee_us.

*   ADD ls_dfin-net        TO ls_fin-netwe.
*   ADD ls_dfin-dry        TO ls_fin-drywe.
*   ADD ls_dfin-mo_grade   TO ls_fin-mo_grade.
*   ADD ls_dfin-mo_content TO ls_fin-mo_content.
*   ADD ls_dfin-re_rate    TO ls_fin-re_rate.
*   ADD ls_dfin-mo_return  TO ls_fin-mo_return.
*   ADD ls_dfin-pro_fee    TO ls_fin-pro_fee.
*   ADD ls_dfin-pro_fee_us TO ls_fin-pro_fee_us.
*   ADD ls_dfin-ave_return TO ls_fin-ave_return.
*   ADD ls_dfin-mo_oxide   TO ls_fin-delivery.
*   ADD 1 TO lv_factor.
*     ls_fin-mo_grade = ls_fin-mo_grade / lv_factor.
*     ls_fin-re_rate  = ls_fin-re_rate  / lv_factor.
 MODIFY gt_diff FROM ls_diff INDEX gv_tabix.

ENDLOOP.

ENDFORM.

***==================================================================***
*              FORM load_volume
***==================================================================***
*              obj. Cargar gt_volume " Volumen diario delivery
***==================================================================***
FORM load_volume. "

  CLEAR: lv_date, lv_final_date.
  lv_date = s_wadat-low.
  ls_volu-month = lv_date+0(6).
  APPEND ls_volu TO gt_volu.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.

   ls_volu-month = lv_final_date+0(6).
   lv_date       = lv_final_date.
   APPEND ls_volu TO gt_volu.

  ENDDO.

  LOOP AT gt_volu INTO ls_volu.
   gv_tabix = sy-tabix.

    LOOP AT gt_ddeli INTO ls_ddeli WHERE dias+0(6) EQ ls_volu-month.

      ADD ls_ddeli-cal1 TO ls_volu-calc1.
      ADD ls_ddeli-cal2 TO ls_volu-calc3.
      ADD ls_ddeli-cal3 TO ls_volu-calc5.
      ADD ls_ddeli-cal4 TO ls_volu-calc7.
      ADD ls_ddeli-cal5 TO ls_volu-calc9.
      ADD ls_ddeli-mo_content TO ls_volu-mo_content.

    ENDLOOP.

    ls_volu-calc2  = ls_volu-calc1  / ls_volu-mo_content. " Ratio1
    ls_volu-calc5  = ls_volu-calc4  / ls_volu-mo_content. " Ratio2
    ls_volu-calc6  = ls_volu-calc5  / ls_volu-mo_content. " Ratio3
    ls_volu-calc8  = ls_volu-calc7  / ls_volu-mo_content. " Ratio4
    ls_volu-calc10 = ls_volu-calc9  / ls_volu-mo_content. " Ratio5

    MODIFY gt_volu FROM ls_volu INDEX gv_tabix.
  ENDLOOP.

ENDFORM.
***==================================================================***
*      FORM  load_dvolume.
***==================================================================***
*      obj. cargar diario volume delivery
***==================================================================***
FORM load_dvolume.

LOOP AT gt_lips INTO ls_lips.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_lips-vbeln BINARY SEARCH.

     ls_dvolu-dias  = ls_likp-wadat_ist.
     ls_dvolu-vbeln = ls_likp-vbeln.
     ls_dvolu-charg = ls_lips-charg.
     ls_dvolu-matnr = ls_lips-matnr.

   CLEAR: ls_ddeli.
   READ TABLE gt_ddeli INTO ls_ddeli WITH KEY dias  = ls_dvolu-dias
                                              vbeln = ls_dvolu-vbeln
                                              charg = ls_dvolu-charg.
    ls_dvolu-cal1 = ls_ddeli-cal1.
    ls_dvolu-cal2 = ls_ddeli-cal2.
    ls_dvolu-cal3 = ls_ddeli-cal3.
    ls_dvolu-cal4 = ls_ddeli-cal4.
    ls_dvolu-cal5 = ls_ddeli-cal5.

    APPEND ls_dvolu TO gt_dvolu.
ENDLOOP.

ENDFORM.
***==================================================================***
*              FORM load_volfin
***==================================================================***
*              obj. Cargar gt_volfin Volumen Final
***==================================================================***
FORM load_volfin. "

  lv_date = s_wadat-low.
  ls_volfin-month = lv_date+0(6).
  APPEND ls_volfin TO gt_volfin.

 DO e_months TIMES.
 DATA:final_date TYPE d.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = final_date.

   ls_volfin-month = final_date+0(6).
   lv_date         = final_date.
   APPEND ls_volfin TO gt_volfin.

  ENDDO.


  LOOP AT gt_volfin INTO ls_volfin.
   gv_tabix = sy-tabix.

    LOOP AT gt_dfin INTO ls_dfin WHERE dias+0(6) EQ ls_volfin-month.

      ADD ls_dfin-cal1 TO ls_volfin-calc1.
      ADD ls_dfin-cal2 TO ls_volfin-calc3.
      ADD ls_dfin-cal3 TO ls_volfin-calc5.
      ADD ls_dfin-cal4 TO ls_volfin-calc7.
      ADD ls_dfin-cal5 TO ls_volfin-calc9.
      ADD ls_dfin-mo_content TO ls_volfin-mo_content.

    ENDLOOP.

      ls_volfin-calc2  = ls_volfin-calc1  / ls_volfin-mo_content. " Ratio1
      ls_volfin-calc5  = ls_volfin-calc4  / ls_volfin-mo_content. " Ratio2
      ls_volfin-calc6  = ls_volfin-calc5  / ls_volfin-mo_content. " Ratio3
      ls_volfin-calc8  = ls_volfin-calc7  / ls_volfin-mo_content. " Ratio4
      ls_volfin-calc10 = ls_volfin-calc9  / ls_volfin-mo_content. " Ratio5

    MODIFY gt_volfin FROM ls_volfin INDEX gv_tabix.

  ENDLOOP.


ENDFORM.
***==================================================================***
*                    form load_dvolfin
***==================================================================***
*                    Cargar tabla gt_dvoldin Volumen diario final
***==================================================================***
FORM load_dvolfin. "

 LOOP AT gt_m101 INTO ls_m101.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_m101-vbeln BINARY SEARCH.

    ls_dvolfin-dias  = ls_likp-wadat_ist.
    ls_dvolfin-vbeln = ls_likp-vbeln.
    ls_dvolfin-charg = ls_m101-charg.
    ls_dvolfin-matnr = ls_m101-matnr.

    CLEAR: ls_dfin.
    READ TABLE gt_dfin INTO ls_dfin WITH KEY dias  = ls_dvolfin-dias
                                             vbeln = ls_dvolfin-vbeln
                                             charg = ls_dvolfin-charg.
    ls_dvolfin-cal1 = ls_dfin-cal1.
    ls_dvolfin-cal2 = ls_dfin-cal2.
    ls_dvolfin-cal3 = ls_dfin-cal3.
    ls_dvolfin-cal4 = ls_dfin-cal4.
    ls_dvolfin-cal5 = ls_dfin-cal5.

    APPEND ls_dvolfin TO gt_dvolfin.

 ENDLOOP.

ENDFORM.

***==================================================================***
*              FORM load_pdeli
***==================================================================***
*              obj. Cargar gt_pdeli Penlaties Delivery
***==================================================================***
FORM load_pdeli.

  CLEAR: lv_date, lv_final_date.
  lv_date = s_wadat-low.
  ls_pdeli-month = lv_date+0(6).
  APPEND ls_pdeli TO gt_pdeli.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.

   ls_pdeli-month  = lv_final_date+0(6).
   lv_date         = lv_final_date.
   APPEND ls_pdeli TO gt_pdeli.
  ENDDO.

LOOP AT gt_pdeli INTO ls_pdeli.
  CLEAR: lv_factor , gv_tabix.
  gv_tabix = sy-tabix.

  LOOP AT gt_pddeli INTO ls_pddeli WHERE dias+0(6) EQ ls_pdeli-month.

     ADD ls_pddeli-mo    TO ls_pdeli-mo.
     ADD ls_pddeli-cu    TO ls_pdeli-cu.
     ADD ls_pddeli-ca    TO ls_pdeli-ca.
     ADD ls_pddeli-na2o  TO ls_pdeli-na2o.
     ADD ls_pddeli-as    TO ls_pdeli-as.
     ADD ls_pddeli-h2o   TO ls_pdeli-h2o.
     ADD ls_pddeli-total TO ls_pdeli-total.
  "   ADD 1 TO lv_factor.

  ENDLOOP.

*  ls_pdeli-mo    = ls_pdeli-mo    / lv_factor.
*  ls_pdeli-cu    = ls_pdeli-cu    / lv_factor.
*  ls_pdeli-ca    = ls_pdeli-ca    / lv_factor.
*  ls_pdeli-na2o  = ls_pdeli-na2o  / lv_factor.
*  ls_pdeli-as    = ls_pdeli-as    / lv_factor.
*  ls_pdeli-h2o   = ls_pdeli-h2o   / lv_factor.
*  ls_pdeli-total = ls_pdeli-total / lv_factor.

  MODIFY gt_pdeli FROM ls_pdeli INDEX gv_tabix.

ENDLOOP.

ENDFORM.

***==================================================================***
*              FORM load_pddeli
***==================================================================***
*              obj. Cargar gt_volume "Penlalties Diario Delivery
***==================================================================***
FORM load_pddeli.

  LOOP AT gt_lips INTO ls_lips.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_lips-vbeln BINARY SEARCH.

   ls_pddeli-dias  = ls_likp-wadat_ist.
   ls_pddeli-vbeln = ls_likp-vbeln.
   ls_pddeli-charg = ls_lips-charg.
   ls_pddeli-matnr = ls_lips-matnr.

*   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input = ls_lips-matnr
*    IMPORTING
*      output = ls_pddeli-matnr.

  CLEAR:    lv_mo,
            lv_cu,
            lv_ca,
            lv_na2o,
            lv_as,
            lv_h2o,
            lv_total,
            lv_dias_retorno.

  CALL FUNCTION 'ZQM013_PENALIDADES'
   EXPORTING
     i_lote                 = ls_pddeli-charg
     i_matnr                = ls_pddeli-matnr
    " i_finales              =
  IMPORTING
     p_mo                   = lv_mo
     p_cu                   = lv_cu
     p_ca                   = lv_ca
     p_na2o                 = lv_na2o
     p_as                   = lv_as
     p_h2o                  = lv_h2o
     p_total                = lv_total
     p_t_dias_retorno       = lv_dias_retorno.

     ls_pddeli-mo    = lv_mo.
     ls_pddeli-cu    = lv_cu.
     ls_pddeli-ca    = lv_ca.
     ls_pddeli-na2o  = lv_na2o.
     ls_pddeli-as    = lv_as.
     ls_pddeli-h2o   = lv_h2o.
     ls_pddeli-total = lv_total.

     APPEND ls_pddeli TO gt_pddeli.
   ENDLOOP.

ENDFORM."load__pddeli

***==================================================================***
*              FORM load_pdprov
***==================================================================***
*              obj. Cargar gt_pdprov Penlaties Diario Provisional
***==================================================================***
FORM load_pdprov.

  LOOP AT gt_m101 INTO ls_m101.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_m101-vbeln BINARY SEARCH.

   ls_pdprov-dias  = ls_likp-wadat_ist.
   ls_pdprov-vbeln = ls_likp-vbeln.
   ls_pdprov-charg = ls_m101-charg.
   ls_pdprov-matnr = ls_m101-matnr.

*   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input =  ls_m101-matnr
*    IMPORTING
*      output = ls_pdprov-matnr.

  CLEAR :   lv_mo,
            lv_cu,
            lv_ca,
            lv_na2o,
            lv_as,
            lv_h2o,
            lv_total,
            lv_dias_retorno.

  CALL FUNCTION 'ZQM013_PENALIDADES'
   EXPORTING
     i_lote                 = ls_pdprov-charg
     i_matnr                = ls_pdprov-matnr
    " i_finales              =
  IMPORTING
     p_mo                   = lv_mo
     p_cu                   = lv_cu
     p_ca                   = lv_ca
     p_na2o                 = lv_na2o
     p_as                   = lv_as
     p_h2o                  = lv_h2o
     p_total                = lv_total
     p_t_dias_retorno       = lv_dias_retorno.

     ls_pdprov-mo    = lv_mo.
     ls_pdprov-cu    = lv_cu.
     ls_pdprov-ca    = lv_ca.
     ls_pdprov-na2o  = lv_na2o.
     ls_pdprov-as    = lv_as.
     ls_pdprov-h2o   = lv_h2o.
     ls_pdprov-total = lv_total.

     APPEND ls_pdprov TO gt_pdprov.
   ENDLOOP.

ENDFORM." load_pdprov."

***==================================================================***
*              FORM load_pprov
***==================================================================***
*              obj. Cargar gt_pprov Penlaties Provisional
***==================================================================***
FORM load_pprov.

  lv_date = s_wadat-low.
  ls_pprov-month = lv_date+0(6).
  APPEND ls_pprov TO gt_pprov.

 DO e_months TIMES.

 DATA:final_date TYPE d.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = final_date.

   ls_pprov-month  = final_date+0(6).
   lv_date        = final_date.
   APPEND ls_pprov TO gt_pprov.
  ENDDO.

LOOP AT gt_pprov INTO ls_pprov.
  CLEAR: lv_factor , gv_tabix.
  gv_tabix = sy-tabix.

  LOOP AT gt_pdprov INTO ls_pdprov WHERE dias+0(6) EQ ls_pprov-month.

    ADD ls_pdprov-mo    TO ls_pprov-mo.
    ADD ls_pdprov-cu    TO ls_pprov-cu.
    ADD ls_pdprov-ca    TO ls_pprov-ca.
    ADD ls_pdprov-na2o  TO ls_pprov-na2o.
    ADD ls_pdprov-as    TO ls_pprov-as.
    ADD ls_pdprov-h2o   TO ls_pprov-h2o.
    ADD ls_pdprov-total TO ls_pprov-total.
"    ADD 1 TO lv_factor.

  ENDLOOP.

*  ls_pprov-mo    = ls_pprov-mo    / lv_factor.
*  ls_pprov-cu    = ls_pprov-cu    / lv_factor.
*  ls_pprov-ca    = ls_pprov-ca    / lv_factor.
*  ls_pprov-na2o  = ls_pprov-na2o  / lv_factor.
*  ls_pprov-as    = ls_pprov-as    / lv_factor.
*  ls_pprov-h2o   = ls_pprov-h2o   / lv_factor.
*  ls_pprov-total = ls_pprov-total / lv_factor.

  MODIFY gt_pprov FROM ls_pprov INDEX gv_tabix.

ENDLOOP.

ENDFORM." load pprov.

***==================================================================***
*              FORM load_pdfin
***==================================================================***
*              obj. Cargar gt_pdfin Penalties Diario Final
***==================================================================***
FORM load_pdfin.

  LOOP AT gt_m101 INTO ls_m101.

   CLEAR: ls_likp.
   READ TABLE gt_likp INTO ls_likp WITH KEY vbeln = ls_m101-vbeln BINARY SEARCH.

   ls_pdfin-dias  = ls_likp-wadat_ist.
   ls_pdfin-vbeln = ls_likp-vbeln.
   ls_pdfin-charg = ls_m101-charg.
   ls_pdfin-matnr = ls_m101-matnr.
*
*   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input =  ls_m101-matnr
*    IMPORTING
*      output = ls_pdfin-matnr.


  CLEAR :   lv_mo,
            lv_cu,
            lv_ca,
            lv_na2o,
            lv_as,
            lv_h2o,
            lv_total,
            lv_dias_retorno.

  CALL FUNCTION 'ZQM013_PENALIDADES'
   EXPORTING
     i_lote                 = ls_pdfin-charg
     i_matnr                = ls_pdfin-matnr
    " i_finales              =
  IMPORTING
     p_mo                   = lv_mo
     p_cu                   = lv_cu
     p_ca                   = lv_ca
     p_na2o                 = lv_na2o
     p_as                   = lv_as
     p_h2o                  = lv_h2o
     p_total                = lv_total
     p_t_dias_retorno       = lv_dias_retorno.

     ls_pdfin-mo    = lv_mo.
     ls_pdfin-cu    = lv_cu.
     ls_pdfin-ca    = lv_ca.
     ls_pdfin-na2o  = lv_na2o.
     ls_pdfin-as    = lv_as.
     ls_pdfin-h2o   = lv_h2o.
     ls_pdfin-total = lv_total.

     APPEND ls_pdfin TO gt_pdfin.
   ENDLOOP.


ENDFORM. " load_pdfin.
***==================================================================***
*              FORM load_pfin
***==================================================================***
*              obj. Cargar gt_pfin " Penalties Final
***==================================================================***
FORM load_pfin.
  CLEAR: lv_date, lv_final_date.

  lv_date = s_wadat-low.
  ls_pfin-month = lv_date+0(6).
  APPEND ls_pfin TO gt_pfin.

 DO e_months TIMES.

 CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date            = lv_date
    days            = 0
    months          = 1
    signum          = '+'
    years           = 0
IMPORTING
   calc_date       = lv_final_date.

   ls_pfin-month  = lv_final_date+0(6).
   lv_date        = lv_final_date.
   APPEND ls_pfin TO gt_pfin.
  ENDDO.

LOOP AT gt_pfin INTO ls_pfin.
  CLEAR: lv_factor , gv_tabix.
  gv_tabix = sy-tabix.

  LOOP AT gt_pdfin INTO ls_pdfin WHERE dias+0(6) EQ ls_pfin-month.

    ADD ls_pdfin-mo    TO ls_pfin-mo.
    ADD ls_pdfin-cu    TO ls_pfin-cu.
    ADD ls_pdfin-ca    TO ls_pfin-ca.
    ADD ls_pdfin-na2o  TO ls_pfin-na2o.
    ADD ls_pdfin-as    TO ls_pfin-as.
    ADD ls_pdfin-h2o   TO ls_pfin-h2o.
    ADD ls_pdfin-total TO ls_pfin-total.
   " ADD 1 TO lv_factor.

  ENDLOOP.

*  ls_pfin-mo    = ls_pfin-mo    / lv_factor.
*  ls_pfin-cu    = ls_pfin-cu    / lv_factor.
*  ls_pfin-ca    = ls_pfin-ca    / lv_factor.
*  ls_pfin-na2o  = ls_pfin-na2o  / lv_factor.
*  ls_pfin-as    = ls_pfin-as    / lv_factor.
*  ls_pfin-h2o   = ls_pfin-h2o   / lv_factor.
*  ls_pfin-total = ls_pfin-total / lv_factor.

  MODIFY gt_pfin FROM ls_pfin INDEX gv_tabix.

ENDLOOP.


ENDFORM. " load_pfin.


* Validamos que la diferencia de años no sea mayor a 1.
*01, 12* = 13+
*05, 01  = 6
*05,

*
*IF lv_num EQ 1 OR lv_num EQ 0.
*
*
*
*ELSE.
*
*  MESSAGE 'Solo rangos con un año de diferencia' TYPE 'S'.
*EXIT.
*ENDIF.
*IF s_wadat-high IS NOT INITIAL.
* IF s_wadat-low+0(4) <> s_wadat-high+0(4).
*  MESSAGE 'Ingrese rangos del mismo año' TYPE 'S'.
* EXIT.
* ENDIF.
*ELSE.
*   MESSAGE 'Ingrese un rango de fechas' TYPE 'S'.
*   EXIT.
*ENDIF.
