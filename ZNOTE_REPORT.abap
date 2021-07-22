********************************************************************
********************************************************************
***   Programa : ZNOTE_REPORT
***   Descrição: Relatório de nota fiscal
***   Autor    : Gleilson Alves
********************************************************************
********************************************************************

REPORT  zmm_note_report.

TABLES: j_1bnfdoc, j_1bnflin, j_1bnfstx, ekes.

TYPE-POOLS slis.

*** Definindo os tipos das tabelas internas
TYPES:  BEGIN OF ty_saida,
          branch     TYPE j_1bnfdoc-branch,
          bukrs      TYPE j_1bnfdoc-bukrs,
          cancel     TYPE j_1bnfdoc-cancel,
          candat     TYPE j_1bnfdoc-candat,
          crenam     TYPE j_1bnfdoc-crenam,
          direct     TYPE j_1bnfdoc-direct,
          docdat     TYPE j_1bnfdoc-docdat,
          docnum     TYPE j_1bnfdoc-docnum,
          docref     TYPE j_1bnfdoc-docref,
          doctyp     TYPE j_1bnfdoc-doctyp,
          gjahr      TYPE j_1bnfdoc-gjahr,
          model      TYPE j_1bnfdoc-model,
          nfenum     TYPE j_1bnfdoc-nfenum,
          nftot      TYPE j_1bnfdoc-nftot,
          nftype     TYPE j_1bnfdoc-nftype,
          parid      TYPE j_1bnfdoc-parid,
          pstdat     TYPE j_1bnfdoc-pstdat,
          series     TYPE j_1bnfdoc-series,
          cfop       TYPE j_1bnflin-cfop,
          docref_lin TYPE j_1bnflin-docref,
          itmnum     TYPE j_1bnflin-itmnum,
          itmref     TYPE j_1bnflin-itmref,
          maktx      TYPE j_1bnflin-maktx,
          matnr      TYPE j_1bnflin-matnr,
          menge      TYPE j_1bnflin-menge,
          netwr      TYPE j_1bnflin-netwr,
          nfnet      TYPE j_1bnflin-nfnet,
          nitemped   TYPE j_1bnflin-nitemped,
          werks      TYPE j_1bnflin-werks,
          xped       TYPE j_1bnflin-xped,
          base       TYPE j_1bnfstx-base,
          excbas     TYPE j_1bnfstx-excbas,
          othbas     TYPE j_1bnfstx-othbas,
          rate       TYPE j_1bnfstx-rate,
          taxtyp     TYPE j_1bnfstx-taxtyp,
          taxval     TYPE j_1bnfstx-taxval,
          xblnr      TYPE ekes-xblnr,
          vbelp      TYPE ekes-vbelp,
        END OF ty_saida,

        BEGIN OF ty_j_1bnfdoc,
          docnum     TYPE j_1bnfdoc-docnum,
          nftype     TYPE j_1bnfdoc-nftype,
          doctyp     TYPE j_1bnfdoc-doctyp,
          direct     TYPE j_1bnfdoc-direct,
          docdat     TYPE j_1bnfdoc-docdat,
          pstdat     TYPE j_1bnfdoc-pstdat,
          crenam     TYPE j_1bnfdoc-crenam,
          model      TYPE j_1bnfdoc-model,
          series     TYPE j_1bnfdoc-series,
          gjahr      TYPE j_1bnfdoc-gjahr,
          bukrs      TYPE j_1bnfdoc-bukrs,
          branch     TYPE j_1bnfdoc-branch,
          parid      TYPE j_1bnfdoc-parid,
          cancel     TYPE j_1bnfdoc-cancel,
          candat     TYPE j_1bnfdoc-candat,
          docref     TYPE j_1bnfdoc-docref,
          nftot      TYPE j_1bnfdoc-nftot,
          nfenum     TYPE j_1bnfdoc-nfenum,
        END OF ty_j_1bnfdoc,

        BEGIN OF ty_j_1bnflin,
          docnum     TYPE j_1bnflin-docnum,
          itmnum     TYPE j_1bnflin-itmnum,
          matnr      TYPE j_1bnflin-matnr,
          maktx      TYPE j_1bnflin-maktx,
          docref     TYPE j_1bnflin-docref,
          itmref     TYPE j_1bnflin-itmref,
          cfop       TYPE j_1bnflin-cfop,
          menge      TYPE j_1bnflin-menge,
          netwr      TYPE j_1bnflin-netwr,
          werks      TYPE j_1bnflin-werks,
          nfnet      TYPE j_1bnflin-nfnet,
          xped       TYPE j_1bnflin-xped,
          nitemped   TYPE j_1bnflin-nitemped,
        END OF ty_j_1bnflin,

        BEGIN OF ty_j_1bnfstx,
          docnum     TYPE j_1bnfstx-docnum,
          itmnum     TYPE j_1bnfstx-itmnum,
          taxtyp     TYPE j_1bnfstx-taxtyp,
          base       TYPE j_1bnfstx-base,
          rate       TYPE j_1bnfstx-rate,
          taxval     TYPE j_1bnfstx-taxval,
          excbas     TYPE j_1bnfstx-excbas,
          othbas     TYPE j_1bnfstx-othbas,
        END OF ty_j_1bnfstx,

        BEGIN OF ty_ekes,
          ebeln      TYPE ekes-ebeln,
          xblnr      TYPE ekes-xblnr,
          vbeln      TYPE ekes-vbeln,
          vbelp      TYPE ekes-vbelp,
        END OF ty_ekes.

*** Tabelas internas
DATA: t_j1bnfdoc TYPE TABLE OF ty_j_1bnfdoc,
      t_j1bnflin TYPE TABLE OF ty_j_1bnflin,
      t_j1bnfstx TYPE TABLE OF ty_j_1bnfstx,
      t_ekes     TYPE TABLE OF ty_ekes,
      t_saida    TYPE TABLE OF ty_saida.

*** Woek Areas
DATA: w_j1bnfdoc TYPE ty_j_1bnfdoc,
      w_j1bnflin TYPE ty_j_1bnflin,
      w_j1bnfstx TYPE ty_j_1bnfstx,
      w_ekes     TYPE ty_ekes,
      w_saida    TYPE ty_saida.

*** Estruturas para ALV
DATA:  t_fieldcat TYPE slis_t_fieldcat_alv,
       w_fieldcat TYPE slis_fieldcat_alv,
       w_layout   TYPE slis_layout_alv.

*** Tela de seleção
SELECTION-SCREEN BEGIN OF BLOCK blc1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_branch FOR j_1bnfdoc-branch,
                s_bukrs  FOR j_1bnfdoc-bukrs,
                s_direct FOR j_1bnfdoc-direct,
                s_docnum FOR j_1bnfdoc-docnum,
                s_gjahr  FOR j_1bnfdoc-gjahr,
                s_nfenum FOR j_1bnfdoc-nfenum,
                s_nftype FOR j_1bnfdoc-nftype,
                s_parid  FOR j_1bnfdoc-parid,
                s_pstdat FOR j_1bnfdoc-pstdat,
                s_cfop   FOR j_1bnflin-cfop,
                s_matnr  FOR j_1bnflin-matnr,
                s_werks  FOR j_1bnflin-werks,
                s_xped   FOR j_1bnflin-xped.
SELECTION-SCREEN END OF BLOCK blc1.

START-OF-SELECTION.

  PERFORM seleciona_dados.
  PERFORM tratar_dados.
  PERFORM montar_alv.

FORM seleciona_dados.

  DATA: lt_ekes_aux TYPE TABLE OF ty_ekes.

  DATA: ls_ekes_aux TYPE ty_ekes.

  DATA: lv_ebeln TYPE ebeln.

  SELECT  docnum
          nftype
          doctyp
          direct
          docdat
          pstdat
          crenam
          model
          series
          gjahr
          bukrs
          branch
          parid
          cancel
          candat
          docref
          nftot
          nfenum
    FROM j_1bnfdoc
    INTO TABLE t_j1bnfdoc
    WHERE docnum IN s_docnum
      AND nftype IN s_nftype
      AND direct IN s_direct
      AND pstdat IN s_pstdat
      AND gjahr  IN s_gjahr
      AND bukrs  IN s_bukrs
      AND branch IN s_branch
      AND parid  IN s_parid
      AND nfenum IN s_nfenum.

  IF sy-subrc IS INITIAL.
    SORT t_j1bnfdoc BY docnum.

    SELECT docnum
           itmnum
           matnr
           maktx
           docref
           itmref
           cfop
           menge
           netwr
           werks
           nfnet
           xped
           nitemped
      FROM j_1bnflin
      INTO TABLE t_j1bnflin
      FOR ALL ENTRIES IN t_j1bnfdoc
      WHERE docnum EQ t_j1bnfdoc-docnum
        AND matnr  IN s_matnr
        AND cfop   IN s_cfop
        AND werks  IN s_werks
        AND xped   IN s_xped.

    IF t_j1bnflin[] IS NOT INITIAL.
      SORT t_j1bnflin BY docnum itmnum.

      SELECT docnum
             itmnum
             taxtyp
             base
             rate
             taxval
             excbas
             othbas
        FROM j_1bnfstx
        INTO TABLE t_j1bnfstx
        FOR ALL ENTRIES IN t_j1bnflin
        WHERE docnum EQ t_j1bnflin-docnum
          AND itmnum EQ t_j1bnflin-itmnum.

      IF t_j1bnfstx[] IS NOT INITIAL.
        SORT t_j1bnfstx BY docnum itmnum taxtyp.
      ENDIF.

      LOOP AT t_j1bnflin INTO w_j1bnflin.
        lv_ebeln = w_j1bnflin-xped.

        SELECT ebeln
               xblnr
               vbeln
               vbelp
          FROM ekes
          INTO TABLE lt_ekes_aux
          WHERE ebeln EQ lv_ebeln.

        IF sy-subrc IS INITIAL.
          READ TABLE lt_ekes_aux INTO ls_ekes_aux INDEX 1.
          MOVE-CORRESPONDING ls_ekes_aux TO w_ekes.
          APPEND w_ekes TO t_ekes.

          CLEAR: w_ekes, ls_ekes_aux.
        ENDIF.
      ENDLOOP.

      SORT t_ekes by ebeln.
    ENDIF.

  ELSE.
    MESSAGE text-ms1 TYPE 'I'. "Nenhum registro encontrado
    STOP.
  ENDIF.

ENDFORM.


FORM tratar_dados.

  DATA: lv_ebeln TYPE ebeln.

  CLEAR w_j1bnflin.

  LOOP AT t_j1bnflin INTO w_j1bnflin.

    CLEAR: lv_ebeln, w_j1bnfdoc, w_j1bnfstx, w_ekes.

    READ TABLE t_j1bnfdoc INTO w_j1bnfdoc
                          WITH KEY docnum = w_j1bnflin-docnum
                          BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      w_saida-branch = w_j1bnfdoc-branch.
      w_saida-bukrs  = w_j1bnfdoc-bukrs.
      w_saida-cancel = w_j1bnfdoc-cancel.
      w_saida-candat = w_j1bnfdoc-candat.
      w_saida-crenam = w_j1bnfdoc-crenam.
      w_saida-direct = w_j1bnfdoc-direct.
      w_saida-docdat = w_j1bnfdoc-docdat.
      w_saida-docnum = w_j1bnfdoc-docnum.
      w_saida-docref = w_j1bnfdoc-docref.
      w_saida-doctyp = w_j1bnfdoc-doctyp.
      w_saida-gjahr  = w_j1bnfdoc-gjahr.
      w_saida-model  = w_j1bnfdoc-model.
      w_saida-nfenum = w_j1bnfdoc-nfenum.
      w_saida-nftot  = w_j1bnfdoc-nftot.
      w_saida-nftype = w_j1bnfdoc-nftype.
      w_saida-parid  = w_j1bnfdoc-parid.
      w_saida-pstdat = w_j1bnfdoc-pstdat.
      w_saida-series = w_j1bnfdoc-series.
    ENDIF.

    READ TABLE t_j1bnfstx INTO w_j1bnfstx
                          WITH KEY docnum = w_j1bnflin-docnum
                                   itmnum = w_j1bnflin-itmnum
                                   BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      w_saida-base   = w_j1bnfstx-base.
      w_saida-excbas = w_j1bnfstx-excbas.
      w_saida-othbas = w_j1bnfstx-othbas.
      w_saida-rate   = w_j1bnfstx-rate.
      w_saida-taxtyp = w_j1bnfstx-taxtyp.
      w_saida-taxval = w_j1bnfstx-taxval.
    ENDIF.

    lv_ebeln = w_j1bnflin-xped.

    READ TABLE t_ekes INTO w_ekes
                      WITH KEY ebeln = lv_ebeln
                      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      w_saida-xblnr = w_ekes-xblnr.
      w_saida-vbelp = w_ekes-vbelp.
    ENDIF.

    w_saida-cfop = w_j1bnflin-cfop.
    w_saida-docref_lin = w_j1bnflin-docref.
    w_saida-itmnum = w_j1bnflin-itmnum.
    w_saida-itmref = w_j1bnflin-itmref.
    w_saida-maktx = w_j1bnflin-maktx.
    w_saida-matnr = w_j1bnflin-matnr.
    w_saida-menge = w_j1bnflin-menge.
    w_saida-netwr = w_j1bnflin-netwr.
    w_saida-nfnet = w_j1bnflin-nfnet.
    w_saida-nitemped = w_j1bnflin-nitemped.
    w_saida-werks = w_j1bnflin-werks.
    w_saida-xped = w_j1bnflin-xped.

    APPEND w_saida TO t_saida.

  ENDLOOP.

ENDFORM.

FORM montar_alv.
  PERFORM monta_fieldcat.
  PERFORM definir_layout.
  PERFORM chamar_alv.
ENDFORM.

FORM monta_fieldcat.

  PERFORM f_fieldcat USING: 'BRANCH' 1 text-f01,
                            'BUKRS'  2 text-f02,
                            'CANCEL' 3 text-f03,
                            'CANDAT' 4 text-f04,
                            'CRENAM' 5 text-f05,
                            'DIRECT' 6 text-f06,
                            'DOCDAT' 7 text-f07,
                            'DOCNUM' 8 text-f08,
                            'DOCREF' 9 text-f09,
                            'DOCTYP' 10 text-f10,
                            'GJAHR'  11 text-f11,
                            'MODEL'  12 text-f12,
                            'NFENUM' 13 text-f13,
                            'NFTOT'  14 text-f14,
                            'NFTYPE' 15 text-f15,
                            'PARID'  16 text-f16,
                            'PSTDAT' 17 text-f17,
                            'SERIES' 18 text-f18,
                            'CFOP'   19 text-f19,
                            'DOCREF_LIN' 20 text-f20,
                            'ITMNUM' 21 text-f21,
                            'ITEMREF' 22 text-f22,
                            'MAKTX'  23 text-f23,
                            'MATNR'  24 text-f24,
                            'MENGE'  25 text-f25,
                            'NETWR'  26 text-f26,
                            'NFNET'  27 text-f27,
                            'NITEMPED' 28 text-f28,
                            'WERKS'  29 text-f29,
                            'XPED'   30 text-f30,
                            'BASE'   31 text-f31,
                            'EXCBAS' 32 text-f32,
                            'OTHBAS' 33 text-f33,
                            'RATE'   34 text-f34,
                            'TAXTYP' 35 text-f35,
                            'TAXVAL' 36 text-f36,
                            'XBLNR'  37 text-f37,
                            'VBELP'  38 text-f38.

ENDFORM.

FORM f_fieldcat USING v_fieldname v_pos v_seltext.

  w_fieldcat-fieldname = v_fieldname.
  w_fieldcat-col_pos   = v_pos.
  w_fieldcat-seltext_m = v_seltext.

  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.

ENDFORM.

FORM definir_layout.

  w_layout-colwidth_optimize = abap_true.
  w_layout-zebra = abap_true.

ENDFORM.

FORM chamar_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = sy-repid
      IS_LAYOUT          = w_layout
      IT_FIELDCAT        = t_fieldcat
    TABLES
      T_OUTTAB           = t_saida
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
