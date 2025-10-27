@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help Sub Project Status'
define view entity ZSUB_PROJ_STA_VH
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZSUB_PROJ_STATUS' )
{
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @UI.hidden: true
  key domain_name,
      @UI.hidden: true
      @UI.lineItem: [{ position: 20, importance: #MEDIUM }]
  key value_position,
      @UI.lineItem: [{ position: 30, importance: #MEDIUM }]
      @Semantics.language: true
      @EndUserText.label: 'Language'
  key language,
      @UI.lineItem: [{ position: 40, importance: #HIGH, label: 'Status' }]
      @UI.identification: [{ label: 'Status' }]
      value_low as Status,
      @UI.lineItem: [{ position: 50, importance: #MEDIUM, label: 'Description' }]
      @Semantics.text: true
      @EndUserText.label: 'Short Description'
      text      as Message
}
