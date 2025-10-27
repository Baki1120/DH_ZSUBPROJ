@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'User Name'
@Search.searchable: true
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_USR_SUB_PROJ_VH
  as select from I_BusinessUserBasic as BusinessUserBasic
{
      @Search.defaultSearchElement: true
  key concat( 'CB', BusinessUserBasic.BusinessPartner ) as BusinessPartner,
      PersonFullName                                    as FullName

}
