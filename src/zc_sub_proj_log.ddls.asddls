@EndUserText.label: 'Change documents'
@Metadata.allowExtensions: true
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_CHANGE_DOCS_SUB_PROJ'
define custom entity ZC_SUB_PROJ_LOG

{
      @UI.hidden         : true
  key objectid           : sysuuid_x16;

      @EndUserText.label : 'Change Number of Document'
      @UI.hidden         : true
  key changenr           : abap.char(10);

      @EndUserText.label : 'Change document creation: Table name  '
      @UI.hidden         : true
  key tabname            : abap.char(30);

      @EndUserText.label : 'Key of Modified Table Row '
      @UI.hidden         : true
  key tabkey             : abap.char(70);

      @EndUserText.label : 'Field Name '
      @UI.hidden         : true
  key fname              : abap.char(30);

      @EndUserText.label : 'Object Description'
      objecttxt          : abap.char(350);

      @EndUserText.label : 'Changed By'
      username           : abap.char(80);

      @EndUserText.label : 'User name of the person resp. in chg doc'
      username_db        : abap.char(12);

      @EndUserText.label : 'Changed On'
      utimestamp         : timestamp;

      @EndUserText.label : 'Creation date of the change document'
      udate              : abap.dats(8);

      @EndUserText.label : 'Creation date of the change document '
      udate_db           : abap.dats(8);

      @EndUserText.label : ' Time changed'
      utime              : abap.tims(6);

      @EndUserText.label : 'Time changed'
      utime_db           : abap.tims(6);

      @EndUserText.label : 'Transaction in which a change was made'
      tcode              : abap.char(20);

      @EndUserText.label : 'Application Object '
      applname           : abap.char(40);

      @EndUserText.label : 'Application Type'
      appltype           : abap.char(2);

      @EndUserText.label : 'Change document creation: Table name db '
      tabname_db         : abap.char(30);

      @EndUserText.label : 'Key of Modified Table Row '
      tabkey_db          : abap.char(70);

      @EndUserText.label : 'Table key length'
      keylen             : abap.numc(4);

      @EndUserText.label : 'Type of Change'
      chngind            : abap.char(1);

      @EndUserText.label : 'Field Name '
      @UI.hidden         : true
      fname_db           : abap.char(30);

      @EndUserText.label : 'Field Name'
      ftext              : abap.char(60);

      @EndUserText.label : 'Create change document: Text type'
      textart            : abap.char(4);

      @EndUserText.label : 'Language Key'
      sprache            : spras;

      @EndUserText.label : 'Text change flag "X"'
      text_case          : abap.char(1);

      @EndUserText.label : 'Output length of the old and new value'
      outlen             : abap.numc(4);

      @EndUserText.label : 'Old Value'
      f_old              : abap.char(254);

      @EndUserText.label : 'Old contents of changed field '
      f_old_db           : abap.char(254);

      @EndUserText.label : 'New Value'
      f_new              : abap.char(254);

      @EndUserText.label : 'New Field Content of Changed Field'
      f_new_db           : abap.char(254);

      @EndUserText.label : 'Old Extended Value (Short)'
      value_shstr_old    : abap.sstring(255);

      @EndUserText.label : 'Old Extended Value (Short)'
      value_shstr_old_db : abap.sstring(255);

      @EndUserText.label : 'New Extended Value (Short)'
      value_shstr_new    : abap.sstring(255);

      @EndUserText.label : 'New Extended Value (Short)'
      value_shstr_new_db : abap.sstring(255);

      @EndUserText.label : 'KEYGUID for Connection to CDPOS_UID'
      keyguid            : abap.char(32);

      @EndUserText.label : 'Key of Modified Table Row'
      tabkey254          : abap.char(254);

      @EndUserText.label : ''
      tabkey254_db       : abap.char(254);

      @EndUserText.label : 'Table key length'
      ext_keylen         : abap.numc(4);

      @EndUserText.label : 'KEYGUID for Link to CDPOS_STR'
      keyguid_str        : abap.char(32);
}
