FUNCTION zzfm_sd_001_address.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_SDI001_REQ OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  .
  TYPES:BEGIN OF ty_uaddr,
          StreetName TYPE string,
        END OF ty_uaddr,
        BEGIN OF ty_udata,
          d TYPE ty_uaddr,
        END OF ty_udata,
        BEGIN OF ty_cdata,
          BusinessPartner TYPE string,
        END OF ty_cdata .
  DATA:lv_json TYPE string.
  DATA:ls_udata TYPE ty_udata.
  DATA:ls_cdata TYPE ty_cdata.
  DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.

  DATA(ls_req) = i_req-req.
  DATA(lo_dest) = zzcl_comm_tool=>get_dest( ).

  lt_mapping = VALUE #(
       ( abap = 'd'                            json = 'd'                    )
       ( abap = 'StreetName'                   json = 'StreetName'         )
     ).

  ls_udata-d-StreetName = ls_req-StreetName.
*&---接口HTTP 链接调用
  TRY.
      DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
      DATA(lo_request) = lo_http_client->get_http_request(   ).
      lo_http_client->enable_path_prefix( ).

      DATA(lv_uri_path) = |/API_BUSINESS_PARTNER/A_BusinessPartnerAddress|.

      lv_uri_path = lv_uri_path && |(BusinessPartner='{ gv_BusinessPartner }',| &&
                                   |AddressID='{ gv_AddressID }')|.

      lo_request->set_uri_path( EXPORTING i_uri_path = lv_uri_path ).
      lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
      lo_request->set_header_field( i_name = 'If-Match' i_value = '*' ).
      lo_http_client->set_csrf_token(  ).

      lo_request->set_content_type( 'application/json' ).
      "传入数据转JSON
      lv_json = /ui2/cl_json=>serialize(
            data          = ls_udata
            compress      = abap_true
            name_mappings = lt_mapping ).

      lo_request->set_text( lv_json ).

*&---执行http post 方法
      DATA(lo_response) = lo_http_client->execute( if_web_http_client=>patch ).
*&---获取http reponse 数据
      DATA(lv_res) = lo_response->get_text(  ).
*&---确定http 状态
      DATA(status) = lo_response->get_status( ).
      IF status-code <> '204'.
        DATA:ls_rese TYPE zzs_odata_fail.
        /ui2/cl_json=>deserialize( EXPORTING json  = lv_res
                                    CHANGING data  = ls_rese ).
        o_resp-msgty = 'E'.
        o_resp-msgtx = '地址信息更新失败' &&  ls_rese-error-message-value .
      ENDIF.

      lo_http_client->close( ).
      FREE:lo_http_client,lo_request,lv_uri_path,lo_request.

    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      RETURN.
  ENDTRY.



ENDFUNCTION.
