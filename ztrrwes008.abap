*----------------------------------------------------------------------*
*                         TREINAMENTO                                  *
*----------------------------------------------------------------------*
* Autor....: Wesley Constantino dos Santos                             *
* Data.....: 12.12.2022                                                *
* Módulo...: TR                                                        *
* Descrição: Testes                                                    *
*----------------------------------------------------------------------*
*                    Histórico das Alterações                          *
*----------------------------------------------------------------------*
* DATA      | AUTOR             | Request    | DESCRIÇÃO               *
*----------------------------------------------------------------------*
*           |                   |            |                         *
*----------------------------------------------------------------------*
REPORT ztrrwes008.

*&---------------------------------------------------------------------*
*                                 TYPES                                *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_download,
         linha(2000) TYPE c,
       END   OF ty_download.

*&---------------------------------------------------------------------*
*                              Variãveis                               *
*&---------------------------------------------------------------------*
DATA: v_evdat     TYPE cciht_ial-evdat,
      vg_filename TYPE string.

*&---------------------------------------------------------------------*
*                        Tabelas Internas                              *
*&---------------------------------------------------------------------*
DATA: t_out      TYPE TABLE OF cciht_ial,
      "t_fieldcat TYPE slis_t_fieldcat_alv,
      t_download TYPE TABLE OF ty_download.

*&---------------------------------------------------------------------*
*                          Workareas                                   *
*&---------------------------------------------------------------------*
DATA: wa_out      LIKE LINE OF t_out,
      " wa_layout   TYPE slis_layout_alv,
      wa_download TYPE ty_download.

*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_evdat FOR v_evdat.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t02.
*Radio Buttons
PARAMETERS: rb_alv  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND cmd,
            rb_dwld RADIOBUTTON GROUP g1.
*Imput do caminho para download
PARAMETERS: p_dwld LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN END OF BLOCK b0.

*Para trazer o caminho do download
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dwld.
  PERFORM zf_seleciona_diretorio.

*Início da execusão
START-OF-SELECTION.
  PERFORM zf_select.

  IF rb_alv = 'X'.
    PERFORM zf_exibe_alv_poo.
  ELSE.
     PERFORM: zf_msg_caminho_vazio,
              zf_prepara_download.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT
*&---------------------------------------------------------------------*
FORM zf_select.
  SELECT *
    INTO TABLE t_out
    FROM cciht_ial
    WHERE  delflg = 'X'
      AND evdat   IN s_evdat.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s398(00) WITH 'Não há registros!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_exibe_alv_poo
*&---------------------------------------------------------------------*
FORM zf_exibe_alv_poo.

  DATA: lo_table  TYPE REF TO cl_salv_table,  "Acessar a classe "cl_salv_table"
        lo_header TYPE REF TO cl_salv_form_layout_grid.   "Para criação do header

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_table "Tabela local
                             CHANGING t_table = t_out ).

      lo_table->get_functions( )->set_all( abap_true ). "Ativar met codes


      CREATE OBJECT lo_header. "É necessário que criemos o objeto header

      lo_header->create_header_information( row = 1 column = 1 text = 'Relatório ALV' ). "Texto grande do header
      lo_header->add_row( ).


      lo_table->get_display_settings( )->set_striped_pattern( abap_true ).

      lo_table->set_top_of_list( lo_header ).

      lo_table->display( ) . "O dispay é fundamental para a exibição do ALV

    CATCH cx_salv_msg
          cx_root.

      MESSAGE s398(00) WITH 'Erro ao exibir tabela' DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_seleciona_diretorio
*&---------------------------------------------------------------------*
FORM zf_seleciona_diretorio .

*Search help para selecionar um directorio.
  DATA: path_str TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Selecione Diretório'
    CHANGING
      selected_folder = path_str
    EXCEPTIONS
      cntl_error      = 1.

  p_dwld = path_str. "Passo o que tem em path_str para p_dwld; meu parametro de entrada do caminho de download

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARA_DOWNLOAD
*&---------------------------------------------------------------------*
FORM zf_prepara_download .

  "DATA: vl_cor(3)    TYPE c, "Crio variáveis do tipo string para receber dados numéricos
  "      vl_valor(12) TYPE c.

  LOOP AT t_out INTO wa_out.
    "   vl_cor = wa_out-cor.  "As variáveis que eram numéricas se tornam strings
    "   vl_valor = wa_out-valor.

    CONCATENATE wa_out-acloc "Concatenate só aceita strings
                wa_out-acloc
                wa_out-aclocdesc
                wa_out-actn
                wa_out-aennr
                wa_out-blockflg
                wa_out-cntrflg
                wa_out-crdat
                wa_out-crnam
                wa_out-delflg
                wa_out-dmtype
                wa_out-eqdesc
                wa_out-equnr
                wa_out-evdat
                wa_out-evdesc
                wa_out-evtime
                wa_out-evtimezone
                wa_out-fatalflg
                wa_out-flplant
                wa_out-ialid
                wa_out-iaplant
                wa_out-iarephdflg
                wa_out-iastatus
                wa_out-iatype
                wa_out-invresult
                wa_out-invresultflg
                wa_out-mandt
                wa_out-mtnlangu
                wa_out-objnr
                wa_out-ownid
                wa_out-parkflg
                wa_out-pmflg
                wa_out-prodlflg
                wa_out-recn
                wa_out-recnroot
                wa_out-recntwah
                wa_out-srsid
                wa_out-tplnr
                wa_out-upddat
                wa_out-updnam
                wa_out-valfr
                wa_out-valto
               " vl_valor   "Passo a variável convertida na posição desejada
               " vl_cor
                INTO  wa_download-linha SEPARATED BY ';'.

    APPEND wa_download TO t_download.
    CLEAR  wa_download.

  ENDLOOP.

  IF NOT t_download[] IS INITIAL.
    PERFORM zf_seleciona_diretorio_saida.
  ELSE.
    MESSAGE e398(00) WITH text-003.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DIRETORIO_SAIDA
*&---------------------------------------------------------------------*
FORM zf_seleciona_diretorio_saida .

  CONCATENATE p_dwld '\' 'Tabela ' '.csv' INTO  vg_filename.

*Chamando a função de download, passando a minha tabela tratada e o nome do arquivo.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = vg_filename     "Passa passar o falename
      filetype                = 'ASC'
    TABLES
      data_tab                = t_download      "Tabela interna de saída
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc IS INITIAL.
    MESSAGE s398(00) WITH text-008.
  ELSE.
    MESSAGE s398(00) WITH text-009 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DIRETORIO_SAIDA
*&---------------------------------------------------------------------*
FORM zf_msg_caminho_vazio .
  IF p_dwld IS INITIAL.
    MESSAGE s398(00) WITH 'Informe um diretório para poder prosseguir!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
