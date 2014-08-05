unit Deltics.ADO.TypeLibrary;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 08-Nov-2007 07:04:43 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Common Files\System\ado\msado15.dll (1)
// LIBID: {2A75196C-D9EB-4129-B803-931327F72D5C}
// LCID: 0
// Helpfile: C:\WINDOWS\help\ado270.chm
// HelpString: Microsoft ActiveX Data Objects 2.8 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\STDOLE2.TLB)
// Errors:
//   Hint: TypeInfo 'Property' changed to 'Property_'
//   Hint: TypeInfo 'Record' changed to 'Record_'
//   Hint: Parameter 'Object' of _DynaCollection.Append changed to 'Object_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of Command15.CreateParameter changed to 'Type_'
//   Hint: Parameter 'Type' of Fields.Append changed to 'Type_'
//   Hint: Parameter 'Type' of Fields20._Append changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Error creating palette bitmap of (TCoConnection) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
//   Error creating palette bitmap of (TCoRecord) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
//   Error creating palette bitmap of (TCoStream) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
//   Error creating palette bitmap of (TCoCommand) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
//   Error creating palette bitmap of (TCoRecordset) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
//   Error creating palette bitmap of (TCoParameter) : Server C:\Program Files\Common Files\System\ado\msado15.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ADODBMajorVersion = 2;
  ADODBMinorVersion = 8;

  LIBID_ADODB: TGUID = '{2A75196C-D9EB-4129-B803-931327F72D5C}';

  IID__Collection: TGUID = '{00000512-0000-0010-8000-00AA006D2EA4}';
  IID__DynaCollection: TGUID = '{00000513-0000-0010-8000-00AA006D2EA4}';
  IID__ADO: TGUID = '{00000534-0000-0010-8000-00AA006D2EA4}';
  IID_Properties: TGUID = '{00000504-0000-0010-8000-00AA006D2EA4}';
  IID_Property_: TGUID = '{00000503-0000-0010-8000-00AA006D2EA4}';
  IID_Error: TGUID = '{00000500-0000-0010-8000-00AA006D2EA4}';
  IID_Errors: TGUID = '{00000501-0000-0010-8000-00AA006D2EA4}';
  IID_Command15: TGUID = '{00000508-0000-0010-8000-00AA006D2EA4}';
  IID_Connection15: TGUID = '{00000515-0000-0010-8000-00AA006D2EA4}';
  IID__Connection: TGUID = '{00000550-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset15: TGUID = '{0000050E-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset20: TGUID = '{0000054F-0000-0010-8000-00AA006D2EA4}';
  IID_Recordset21: TGUID = '{00000555-0000-0010-8000-00AA006D2EA4}';
  IID__Recordset: TGUID = '{00000556-0000-0010-8000-00AA006D2EA4}';
  IID_Fields15: TGUID = '{00000506-0000-0010-8000-00AA006D2EA4}';
  IID_Fields20: TGUID = '{0000054D-0000-0010-8000-00AA006D2EA4}';
  IID_Fields: TGUID = '{00000564-0000-0010-8000-00AA006D2EA4}';
  IID_Field20: TGUID = '{0000054C-0000-0010-8000-00AA006D2EA4}';
  IID_Field: TGUID = '{00000569-0000-0010-8000-00AA006D2EA4}';
  IID__Parameter: TGUID = '{0000050C-0000-0010-8000-00AA006D2EA4}';
  IID_Parameters: TGUID = '{0000050D-0000-0010-8000-00AA006D2EA4}';
  IID_Command25: TGUID = '{0000054E-0000-0010-8000-00AA006D2EA4}';
  IID__Command: TGUID = '{B08400BD-F9D1-4D02-B856-71D5DBA123E9}';
  IID_ConnectionEventsVt: TGUID = '{00000402-0000-0010-8000-00AA006D2EA4}';
  IID_RecordsetEventsVt: TGUID = '{00000403-0000-0010-8000-00AA006D2EA4}';
  DIID_ConnectionEvents: TGUID = '{00000400-0000-0010-8000-00AA006D2EA4}';
  DIID_RecordsetEvents: TGUID = '{00000266-0000-0010-8000-00AA006D2EA4}';
  IID_ADOConnectionConstruction15: TGUID = '{00000516-0000-0010-8000-00AA006D2EA4}';
  IID_ADOConnectionConstruction: TGUID = '{00000551-0000-0010-8000-00AA006D2EA4}';
  CLASS_Connection: TGUID = '{00000514-0000-0010-8000-00AA006D2EA4}';
  IID__Record: TGUID = '{00000562-0000-0010-8000-00AA006D2EA4}';
  CLASS_Record_: TGUID = '{00000560-0000-0010-8000-00AA006D2EA4}';
  IID__Stream: TGUID = '{00000565-0000-0010-8000-00AA006D2EA4}';
  CLASS_Stream: TGUID = '{00000566-0000-0010-8000-00AA006D2EA4}';
  IID_ADORecordConstruction: TGUID = '{00000567-0000-0010-8000-00AA006D2EA4}';
  IID_ADOStreamConstruction: TGUID = '{00000568-0000-0010-8000-00AA006D2EA4}';
  IID_ADOCommandConstruction: TGUID = '{00000517-0000-0010-8000-00AA006D2EA4}';
  CLASS_Command: TGUID = '{00000507-0000-0010-8000-00AA006D2EA4}';
  CLASS_Recordset: TGUID = '{00000535-0000-0010-8000-00AA006D2EA4}';
  IID_ADORecordsetConstruction: TGUID = '{00000283-0000-0010-8000-00AA006D2EA4}';
  IID_Field15: TGUID = '{00000505-0000-0010-8000-00AA006D2EA4}';
  CLASS_Parameter: TGUID = '{0000050B-0000-0010-8000-00AA006D2EA4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CursorTypeEnum
type
  CursorTypeEnum = TOleEnum;
const
  adOpenUnspecified = $FFFFFFFF;
  adOpenForwardOnly = $00000000;
  adOpenKeyset = $00000001;
  adOpenDynamic = $00000002;
  adOpenStatic = $00000003;

// Constants for enum CursorOptionEnum
type
  CursorOptionEnum = TOleEnum;
const
  adHoldRecords = $00000100;
  adMovePrevious = $00000200;
  adAddNew = $01000400;
  adDelete = $01000800;
  adUpdate = $01008000;
  adBookmark = $00002000;
  adApproxPosition = $00004000;
  adUpdateBatch = $00010000;
  adResync = $00020000;
  adNotify = $00040000;
  adFind = $00080000;
  adSeek = $00400000;
  adIndex = $00800000;

// Constants for enum LockTypeEnum
type
  LockTypeEnum = TOleEnum;
const
  adLockUnspecified = $FFFFFFFF;
  adLockReadOnly = $00000001;
  adLockPessimistic = $00000002;
  adLockOptimistic = $00000003;
  adLockBatchOptimistic = $00000004;

// Constants for enum ExecuteOptionEnum
type
  ExecuteOptionEnum = TOleEnum;
const
  adOptionUnspecified = $FFFFFFFF;
  adAsyncExecute = $00000010;
  adAsyncFetch = $00000020;
  adAsyncFetchNonBlocking = $00000040;
  adExecuteNoRecords = $00000080;
  adExecuteStream = $00000400;
  adExecuteRecord = $00000800;

// Constants for enum ConnectOptionEnum
type
  ConnectOptionEnum = TOleEnum;
const
  adConnectUnspecified = $FFFFFFFF;
  adAsyncConnect = $00000010;

// Constants for enum ObjectStateEnum
type
  ObjectStateEnum = TOleEnum;
const
  adStateClosed = $00000000;
  adStateOpen = $00000001;
  adStateConnecting = $00000002;
  adStateExecuting = $00000004;
  adStateFetching = $00000008;

// Constants for enum CursorLocationEnum
type
  CursorLocationEnum = TOleEnum;
const
  adUseNone = $00000001;
  adUseServer = $00000002;
  adUseClient = $00000003;
  adUseClientBatch = $00000003;

// Constants for enum DataTypeEnum
type
  DataTypeEnum = TOleEnum;
const
  adEmpty = $00000000;
  adTinyInt = $00000010;
  adSmallInt = $00000002;
  adInteger = $00000003;
  adBigInt = $00000014;
  adUnsignedTinyInt = $00000011;
  adUnsignedSmallInt = $00000012;
  adUnsignedInt = $00000013;
  adUnsignedBigInt = $00000015;
  adSingle = $00000004;
  adDouble = $00000005;
  adCurrency = $00000006;
  adDecimal = $0000000E;
  adNumeric = $00000083;
  adBoolean = $0000000B;
  adError = $0000000A;
  adUserDefined = $00000084;
  adVariant = $0000000C;
  adIDispatch = $00000009;
  adIUnknown = $0000000D;
  adGUID = $00000048;
  adDate = $00000007;
  adDBDate = $00000085;
  adDBTime = $00000086;
  adDBTimeStamp = $00000087;
  adBSTR = $00000008;
  adChar = $00000081;
  adVarChar = $000000C8;
  adLongVarChar = $000000C9;
  adWChar = $00000082;
  adVarWChar = $000000CA;
  adLongVarWChar = $000000CB;
  adBinary = $00000080;
  adVarBinary = $000000CC;
  adLongVarBinary = $000000CD;
  adChapter = $00000088;
  adFileTime = $00000040;
  adPropVariant = $0000008A;
  adVarNumeric = $0000008B;
  adArray = $00002000;

// Constants for enum FieldAttributeEnum
type
  FieldAttributeEnum = TOleEnum;
const
  adFldUnspecified = $FFFFFFFF;
  adFldMayDefer = $00000002;
  adFldUpdatable = $00000004;
  adFldUnknownUpdatable = $00000008;
  adFldFixed = $00000010;
  adFldIsNullable = $00000020;
  adFldMayBeNull = $00000040;
  adFldLong = $00000080;
  adFldRowID = $00000100;
  adFldRowVersion = $00000200;
  adFldCacheDeferred = $00001000;
  adFldIsChapter = $00002000;
  adFldNegativeScale = $00004000;
  adFldKeyColumn = $00008000;
  adFldIsRowURL = $00010000;
  adFldIsDefaultStream = $00020000;
  adFldIsCollection = $00040000;

// Constants for enum EditModeEnum
type
  EditModeEnum = TOleEnum;
const
  adEditNone = $00000000;
  adEditInProgress = $00000001;
  adEditAdd = $00000002;
  adEditDelete = $00000004;

// Constants for enum RecordStatusEnum
type
  RecordStatusEnum = TOleEnum;
const
  adRecOK = $00000000;
  adRecNew = $00000001;
  adRecModified = $00000002;
  adRecDeleted = $00000004;
  adRecUnmodified = $00000008;
  adRecInvalid = $00000010;
  adRecMultipleChanges = $00000040;
  adRecPendingChanges = $00000080;
  adRecCanceled = $00000100;
  adRecCantRelease = $00000400;
  adRecConcurrencyViolation = $00000800;
  adRecIntegrityViolation = $00001000;
  adRecMaxChangesExceeded = $00002000;
  adRecObjectOpen = $00004000;
  adRecOutOfMemory = $00008000;
  adRecPermissionDenied = $00010000;
  adRecSchemaViolation = $00020000;
  adRecDBDeleted = $00040000;

// Constants for enum GetRowsOptionEnum
type
  GetRowsOptionEnum = TOleEnum;
const
  adGetRowsRest = $FFFFFFFF;

// Constants for enum PositionEnum
type
  PositionEnum = TOleEnum;
const
  adPosUnknown = $FFFFFFFF;
  adPosBOF = $FFFFFFFE;
  adPosEOF = $FFFFFFFD;

// Constants for enum BookmarkEnum
type
  BookmarkEnum = TOleEnum;
const
  adBookmarkCurrent = $00000000;
  adBookmarkFirst = $00000001;
  adBookmarkLast = $00000002;

// Constants for enum MarshalOptionsEnum
type
  MarshalOptionsEnum = TOleEnum;
const
  adMarshalAll = $00000000;
  adMarshalModifiedOnly = $00000001;

// Constants for enum AffectEnum
type
  AffectEnum = TOleEnum;
const
  adAffectCurrent = $00000001;
  adAffectGroup = $00000002;
  adAffectAll = $00000003;
  adAffectAllChapters = $00000004;

// Constants for enum ResyncEnum
type
  ResyncEnum = TOleEnum;
const
  adResyncUnderlyingValues = $00000001;
  adResyncAllValues = $00000002;

// Constants for enum CompareEnum
type
  CompareEnum = TOleEnum;
const
  adCompareLessThan = $00000000;
  adCompareEqual = $00000001;
  adCompareGreaterThan = $00000002;
  adCompareNotEqual = $00000003;
  adCompareNotComparable = $00000004;

// Constants for enum FilterGroupEnum
type
  FilterGroupEnum = TOleEnum;
const
  adFilterNone = $00000000;
  adFilterPendingRecords = $00000001;
  adFilterAffectedRecords = $00000002;
  adFilterFetchedRecords = $00000003;
  adFilterPredicate = $00000004;
  adFilterConflictingRecords = $00000005;

// Constants for enum SearchDirectionEnum
type
  SearchDirectionEnum = TOleEnum;
const
  adSearchForward = $00000001;
  adSearchBackward = $FFFFFFFF;

// Constants for enum PersistFormatEnum
type
  PersistFormatEnum = TOleEnum;
const
  adPersistADTG = $00000000;
  adPersistXML = $00000001;

// Constants for enum StringFormatEnum
type
  StringFormatEnum = TOleEnum;
const
  adClipString = $00000002;

// Constants for enum ConnectPromptEnum
type
  ConnectPromptEnum = TOleEnum;
const
  adPromptAlways = $00000001;
  adPromptComplete = $00000002;
  adPromptCompleteRequired = $00000003;
  adPromptNever = $00000004;

// Constants for enum ConnectModeEnum
type
  ConnectModeEnum = TOleEnum;
const
  adModeUnknown = $00000000;
  adModeRead = $00000001;
  adModeWrite = $00000002;
  adModeReadWrite = $00000003;
  adModeShareDenyRead = $00000004;
  adModeShareDenyWrite = $00000008;
  adModeShareExclusive = $0000000C;
  adModeShareDenyNone = $00000010;
  adModeRecursive = $00400000;

// Constants for enum RecordCreateOptionsEnum
type
  RecordCreateOptionsEnum = TOleEnum;
const
  adCreateCollection = $00002000;
  adCreateStructDoc = $80000000;
  adCreateNonCollection = $00000000;
  adOpenIfExists = $02000000;
  adCreateOverwrite = $04000000;
  adFailIfNotExists = $FFFFFFFF;

// Constants for enum RecordOpenOptionsEnum
type
  RecordOpenOptionsEnum = TOleEnum;
const
  adOpenRecordUnspecified = $FFFFFFFF;
  adOpenSource = $00800000;
  adOpenOutput = $00800000;
  adOpenAsync = $00001000;
  adDelayFetchStream = $00004000;
  adDelayFetchFields = $00008000;
  adOpenExecuteCommand = $00010000;

// Constants for enum IsolationLevelEnum
type
  IsolationLevelEnum = TOleEnum;
const
  adXactUnspecified = $FFFFFFFF;
  adXactChaos = $00000010;
  adXactReadUncommitted = $00000100;
  adXactBrowse = $00000100;
  adXactCursorStability = $00001000;
  adXactReadCommitted = $00001000;
  adXactRepeatableRead = $00010000;
  adXactSerializable = $00100000;
  adXactIsolated = $00100000;

// Constants for enum XactAttributeEnum
type
  XactAttributeEnum = TOleEnum;
const
  adXactCommitRetaining = $00020000;
  adXactAbortRetaining = $00040000;
  adXactAsyncPhaseOne = $00080000;
  adXactSyncPhaseOne = $00100000;

// Constants for enum PropertyAttributesEnum
type
  PropertyAttributesEnum = TOleEnum;
const
  adPropNotSupported = $00000000;
  adPropRequired = $00000001;
  adPropOptional = $00000002;
  adPropRead = $00000200;
  adPropWrite = $00000400;

// Constants for enum ErrorValueEnum
type
  ErrorValueEnum = TOleEnum;
const
  adErrProviderFailed = $00000BB8;
  adErrInvalidArgument = $00000BB9;
  adErrOpeningFile = $00000BBA;
  adErrReadFile = $00000BBB;
  adErrWriteFile = $00000BBC;
  adErrNoCurrentRecord = $00000BCD;
  adErrIllegalOperation = $00000C93;
  adErrCantChangeProvider = $00000C94;
  adErrInTransaction = $00000CAE;
  adErrFeatureNotAvailable = $00000CB3;
  adErrItemNotFound = $00000CC1;
  adErrObjectInCollection = $00000D27;
  adErrObjectNotSet = $00000D5C;
  adErrDataConversion = $00000D5D;
  adErrObjectClosed = $00000E78;
  adErrObjectOpen = $00000E79;
  adErrProviderNotFound = $00000E7A;
  adErrBoundToCommand = $00000E7B;
  adErrInvalidParamInfo = $00000E7C;
  adErrInvalidConnection = $00000E7D;
  adErrNotReentrant = $00000E7E;
  adErrStillExecuting = $00000E7F;
  adErrOperationCancelled = $00000E80;
  adErrStillConnecting = $00000E81;
  adErrInvalidTransaction = $00000E82;
  adErrNotExecuting = $00000E83;
  adErrUnsafeOperation = $00000E84;
  adwrnSecurityDialog = $00000E85;
  adwrnSecurityDialogHeader = $00000E86;
  adErrIntegrityViolation = $00000E87;
  adErrPermissionDenied = $00000E88;
  adErrDataOverflow = $00000E89;
  adErrSchemaViolation = $00000E8A;
  adErrSignMismatch = $00000E8B;
  adErrCantConvertvalue = $00000E8C;
  adErrCantCreate = $00000E8D;
  adErrColumnNotOnThisRow = $00000E8E;
  adErrURLDoesNotExist = $00000E8F;
  adErrTreePermissionDenied = $00000E90;
  adErrInvalidURL = $00000E91;
  adErrResourceLocked = $00000E92;
  adErrResourceExists = $00000E93;
  adErrCannotComplete = $00000E94;
  adErrVolumeNotFound = $00000E95;
  adErrOutOfSpace = $00000E96;
  adErrResourceOutOfScope = $00000E97;
  adErrUnavailable = $00000E98;
  adErrURLNamedRowDoesNotExist = $00000E99;
  adErrDelResOutOfScope = $00000E9A;
  adErrPropInvalidColumn = $00000E9B;
  adErrPropInvalidOption = $00000E9C;
  adErrPropInvalidValue = $00000E9D;
  adErrPropConflicting = $00000E9E;
  adErrPropNotAllSettable = $00000E9F;
  adErrPropNotSet = $00000EA0;
  adErrPropNotSettable = $00000EA1;
  adErrPropNotSupported = $00000EA2;
  adErrCatalogNotSet = $00000EA3;
  adErrCantChangeConnection = $00000EA4;
  adErrFieldsUpdateFailed = $00000EA5;
  adErrDenyNotSupported = $00000EA6;
  adErrDenyTypeNotSupported = $00000EA7;
  adErrProviderNotSpecified = $00000EA9;
  adErrConnectionStringTooLong = $00000EAA;

// Constants for enum ParameterAttributesEnum
type
  ParameterAttributesEnum = TOleEnum;
const
  adParamSigned = $00000010;
  adParamNullable = $00000040;
  adParamLong = $00000080;

// Constants for enum ParameterDirectionEnum
type
  ParameterDirectionEnum = TOleEnum;
const
  adParamUnknown = $00000000;
  adParamInput = $00000001;
  adParamOutput = $00000002;
  adParamInputOutput = $00000003;
  adParamReturnValue = $00000004;

// Constants for enum CommandTypeEnum
type
  CommandTypeEnum = TOleEnum;
const
  adCmdUnspecified = $FFFFFFFF;
  adCmdUnknown = $00000008;
  adCmdText = $00000001;
  adCmdTable = $00000002;
  adCmdStoredProc = $00000004;
  adCmdFile = $00000100;
  adCmdTableDirect = $00000200;

// Constants for enum EventStatusEnum
type
  EventStatusEnum = TOleEnum;
const
  adStatusOK = $00000001;
  adStatusErrorsOccurred = $00000002;
  adStatusCantDeny = $00000003;
  adStatusCancel = $00000004;
  adStatusUnwantedEvent = $00000005;

// Constants for enum EventReasonEnum
type
  EventReasonEnum = TOleEnum;
const
  adRsnAddNew = $00000001;
  adRsnDelete = $00000002;
  adRsnUpdate = $00000003;
  adRsnUndoUpdate = $00000004;
  adRsnUndoAddNew = $00000005;
  adRsnUndoDelete = $00000006;
  adRsnRequery = $00000007;
  adRsnResynch = $00000008;
  adRsnClose = $00000009;
  adRsnMove = $0000000A;
  adRsnFirstChange = $0000000B;
  adRsnMoveFirst = $0000000C;
  adRsnMoveNext = $0000000D;
  adRsnMovePrevious = $0000000E;
  adRsnMoveLast = $0000000F;

// Constants for enum SchemaEnum
type
  SchemaEnum = TOleEnum;
const
  adSchemaProviderSpecific = $FFFFFFFF;
  adSchemaAsserts = $00000000;
  adSchemaCatalogs = $00000001;
  adSchemaCharacterSets = $00000002;
  adSchemaCollations = $00000003;
  adSchemaColumns = $00000004;
  adSchemaCheckConstraints = $00000005;
  adSchemaConstraintColumnUsage = $00000006;
  adSchemaConstraintTableUsage = $00000007;
  adSchemaKeyColumnUsage = $00000008;
  adSchemaReferentialContraints = $00000009;
  adSchemaReferentialConstraints = $00000009;
  adSchemaTableConstraints = $0000000A;
  adSchemaColumnsDomainUsage = $0000000B;
  adSchemaIndexes = $0000000C;
  adSchemaColumnPrivileges = $0000000D;
  adSchemaTablePrivileges = $0000000E;
  adSchemaUsagePrivileges = $0000000F;
  adSchemaProcedures = $00000010;
  adSchemaSchemata = $00000011;
  adSchemaSQLLanguages = $00000012;
  adSchemaStatistics = $00000013;
  adSchemaTables = $00000014;
  adSchemaTranslations = $00000015;
  adSchemaProviderTypes = $00000016;
  adSchemaViews = $00000017;
  adSchemaViewColumnUsage = $00000018;
  adSchemaViewTableUsage = $00000019;
  adSchemaProcedureParameters = $0000001A;
  adSchemaForeignKeys = $0000001B;
  adSchemaPrimaryKeys = $0000001C;
  adSchemaProcedureColumns = $0000001D;
  adSchemaDBInfoKeywords = $0000001E;
  adSchemaDBInfoLiterals = $0000001F;
  adSchemaCubes = $00000020;
  adSchemaDimensions = $00000021;
  adSchemaHierarchies = $00000022;
  adSchemaLevels = $00000023;
  adSchemaMeasures = $00000024;
  adSchemaProperties = $00000025;
  adSchemaMembers = $00000026;
  adSchemaTrustees = $00000027;
  adSchemaFunctions = $00000028;
  adSchemaActions = $00000029;
  adSchemaCommands = $0000002A;
  adSchemaSets = $0000002B;

// Constants for enum FieldStatusEnum
type
  FieldStatusEnum = TOleEnum;
const
  adFieldOK = $00000000;
  adFieldCantConvertValue = $00000002;
  adFieldIsNull = $00000003;
  adFieldTruncated = $00000004;
  adFieldSignMismatch = $00000005;
  adFieldDataOverflow = $00000006;
  adFieldCantCreate = $00000007;
  adFieldUnavailable = $00000008;
  adFieldPermissionDenied = $00000009;
  adFieldIntegrityViolation = $0000000A;
  adFieldSchemaViolation = $0000000B;
  adFieldBadStatus = $0000000C;
  adFieldDefault = $0000000D;
  adFieldIgnore = $0000000F;
  adFieldDoesNotExist = $00000010;
  adFieldInvalidURL = $00000011;
  adFieldResourceLocked = $00000012;
  adFieldResourceExists = $00000013;
  adFieldCannotComplete = $00000014;
  adFieldVolumeNotFound = $00000015;
  adFieldOutOfSpace = $00000016;
  adFieldCannotDeleteSource = $00000017;
  adFieldReadOnly = $00000018;
  adFieldResourceOutOfScope = $00000019;
  adFieldAlreadyExists = $0000001A;
  adFieldPendingInsert = $00010000;
  adFieldPendingDelete = $00020000;
  adFieldPendingChange = $00040000;
  adFieldPendingUnknown = $00080000;
  adFieldPendingUnknownDelete = $00100000;

// Constants for enum SeekEnum
type
  SeekEnum = TOleEnum;
const
  adSeekFirstEQ = $00000001;
  adSeekLastEQ = $00000002;
  adSeekAfterEQ = $00000004;
  adSeekAfter = $00000008;
  adSeekBeforeEQ = $00000010;
  adSeekBefore = $00000020;

// Constants for enum ADCPROP_UPDATECRITERIA_ENUM
type
  ADCPROP_UPDATECRITERIA_ENUM = TOleEnum;
const
  adCriteriaKey = $00000000;
  adCriteriaAllCols = $00000001;
  adCriteriaUpdCols = $00000002;
  adCriteriaTimeStamp = $00000003;

// Constants for enum ADCPROP_ASYNCTHREADPRIORITY_ENUM
type
  ADCPROP_ASYNCTHREADPRIORITY_ENUM = TOleEnum;
const
  adPriorityLowest = $00000001;
  adPriorityBelowNormal = $00000002;
  adPriorityNormal = $00000003;
  adPriorityAboveNormal = $00000004;
  adPriorityHighest = $00000005;

// Constants for enum ADCPROP_AUTORECALC_ENUM
type
  ADCPROP_AUTORECALC_ENUM = TOleEnum;
const
  adRecalcUpFront = $00000000;
  adRecalcAlways = $00000001;

// Constants for enum ADCPROP_UPDATERESYNC_ENUM
type
  ADCPROP_UPDATERESYNC_ENUM = TOleEnum;
const
  adResyncNone = $00000000;
  adResyncAutoIncrement = $00000001;
  adResyncConflicts = $00000002;
  adResyncUpdates = $00000004;
  adResyncInserts = $00000008;
  adResyncAll = $0000000F;

// Constants for enum MoveRecordOptionsEnum
type
  MoveRecordOptionsEnum = TOleEnum;
const
  adMoveUnspecified = $FFFFFFFF;
  adMoveOverWrite = $00000001;
  adMoveDontUpdateLinks = $00000002;
  adMoveAllowEmulation = $00000004;

// Constants for enum CopyRecordOptionsEnum
type
  CopyRecordOptionsEnum = TOleEnum;
const
  adCopyUnspecified = $FFFFFFFF;
  adCopyOverWrite = $00000001;
  adCopyAllowEmulation = $00000004;
  adCopyNonRecursive = $00000002;

// Constants for enum StreamTypeEnum
type
  StreamTypeEnum = TOleEnum;
const
  adTypeBinary = $00000001;
  adTypeText = $00000002;

// Constants for enum LineSeparatorEnum
type
  LineSeparatorEnum = TOleEnum;
const
  adLF = $0000000A;
  adCR = $0000000D;
  adCRLF = $FFFFFFFF;

// Constants for enum StreamOpenOptionsEnum
type
  StreamOpenOptionsEnum = TOleEnum;
const
  adOpenStreamUnspecified = $FFFFFFFF;
  adOpenStreamAsync = $00000001;
  adOpenStreamFromRecord = $00000004;

// Constants for enum StreamWriteEnum
type
  StreamWriteEnum = TOleEnum;
const
  adWriteChar = $00000000;
  adWriteLine = $00000001;
  stWriteChar = $00000000;
  stWriteLine = $00000001;

// Constants for enum SaveOptionsEnum
type
  SaveOptionsEnum = TOleEnum;
const
  adSaveCreateNotExist = $00000001;
  adSaveCreateOverWrite = $00000002;

// Constants for enum FieldEnum
type
  FieldEnum = TOleEnum;
const
  adDefaultStream = $FFFFFFFF;
  adRecordURL = $FFFFFFFE;

// Constants for enum StreamReadEnum
type
  StreamReadEnum = TOleEnum;
const
  adReadAll = $FFFFFFFF;
  adReadLine = $FFFFFFFE;

// Constants for enum RecordTypeEnum
type
  RecordTypeEnum = TOleEnum;
const
  adSimpleRecord = $00000000;
  adCollectionRecord = $00000001;
  adStructDoc = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _Collection = interface;
  _CollectionDisp = dispinterface;
  _DynaCollection = interface;
  _DynaCollectionDisp = dispinterface;
  _ADO = interface;
  _ADODisp = dispinterface;
  Properties = interface;
  PropertiesDisp = dispinterface;
  Property_ = interface;
  Property_Disp = dispinterface;
  Error = interface;
  ErrorDisp = dispinterface;
  Errors = interface;
  ErrorsDisp = dispinterface;
  Command15 = interface;
  Command15Disp = dispinterface;
  Connection15 = interface;
  Connection15Disp = dispinterface;
  _Connection = interface;
  _ConnectionDisp = dispinterface;
  Recordset15 = interface;
  Recordset15Disp = dispinterface;
  Recordset20 = interface;
  Recordset20Disp = dispinterface;
  Recordset21 = interface;
  Recordset21Disp = dispinterface;
  _Recordset = interface;
  _RecordsetDisp = dispinterface;
  Fields15 = interface;
  Fields15Disp = dispinterface;
  Fields20 = interface;
  Fields20Disp = dispinterface;
  Fields = interface;
  FieldsDisp = dispinterface;
  Field20 = interface;
  Field20Disp = dispinterface;
  Field = interface;
  FieldDisp = dispinterface;
  _Parameter = interface;
  _ParameterDisp = dispinterface;
  Parameters = interface;
  ParametersDisp = dispinterface;
  Command25 = interface;
  Command25Disp = dispinterface;
  _Command = interface;
  _CommandDisp = dispinterface;
  ConnectionEventsVt = interface;
  RecordsetEventsVt = interface;
  ConnectionEvents = dispinterface;
  RecordsetEvents = dispinterface;
  ADOConnectionConstruction15 = interface;
  ADOConnectionConstruction = interface;
  _Record = interface;
  _RecordDisp = dispinterface;
  _Stream = interface;
  _StreamDisp = dispinterface;
  ADORecordConstruction = interface;
  ADOStreamConstruction = interface;
  ADOCommandConstruction = interface;
  ADORecordsetConstruction = interface;
  Field15 = interface;
  Field15Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Connection = _Connection;
  Record_ = _Record;
  Stream = _Stream;
  Command = _Command;
  Recordset = _Recordset;
  Parameter = _Parameter;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}

  PositionEnum_Param = PositionEnum; 
  SearchDirection = SearchDirectionEnum; 
  ADO_LONGPTR = Integer; 

// *********************************************************************//
// Interface: _Collection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Collection = interface(IDispatch)
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    function Get_Count: Integer; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Refresh; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  _CollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000512-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _CollectionDisp = dispinterface
    ['{00000512-0000-0010-8000-00AA006D2EA4}']
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: _DynaCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _DynaCollection = interface(_Collection)
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  _DynaCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000513-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _DynaCollectionDisp = dispinterface
    ['{00000513-0000-0010-8000-00AA006D2EA4}']
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: _ADO
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ADO = interface(IDispatch)
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    function Get_Properties: Properties; safecall;
    property Properties: Properties read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  _ADODisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000534-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ADODisp = dispinterface
    ['{00000534-0000-0010-8000-00AA006D2EA4}']
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Properties
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Properties = interface(_Collection)
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Property_; safecall;
    property Item[Index: OleVariant]: Property_ read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  PropertiesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000504-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  PropertiesDisp = dispinterface
    ['{00000504-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Property_ readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Property_
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Property_ = interface(IDispatch)
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pval: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttributes: Integer); safecall;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  Property_Disp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000503-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Property_Disp = dispinterface
    ['{00000503-0000-0010-8000-00AA006D2EA4}']
    property Value: OleVariant dispid 0;
    property Name: WideString readonly dispid 1610743810;
    property type_: DataTypeEnum readonly dispid 1610743811;
    property Attributes: Integer dispid 1610743812;
  end;

// *********************************************************************//
// Interface: Error
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Error = interface(IDispatch)
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    function Get_Number: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_HelpContext: Integer; safecall;
    function Get_SQLState: WideString; safecall;
    function Get_NativeError: Integer; safecall;
    property Number: Integer read Get_Number;
    property Source: WideString read Get_Source;
    property Description: WideString read Get_Description;
    property HelpFile: WideString read Get_HelpFile;
    property HelpContext: Integer read Get_HelpContext;
    property SQLState: WideString read Get_SQLState;
    property NativeError: Integer read Get_NativeError;
  end;

// *********************************************************************//
// DispIntf:  ErrorDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000500-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ErrorDisp = dispinterface
    ['{00000500-0000-0010-8000-00AA006D2EA4}']
    property Number: Integer readonly dispid 1;
    property Source: WideString readonly dispid 2;
    property Description: WideString readonly dispid 0;
    property HelpFile: WideString readonly dispid 3;
    property HelpContext: Integer readonly dispid 4;
    property SQLState: WideString readonly dispid 5;
    property NativeError: Integer readonly dispid 6;
  end;

// *********************************************************************//
// Interface: Errors
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Errors = interface(_Collection)
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Error; safecall;
    procedure Clear; safecall;
    property Item[Index: OleVariant]: Error read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  ErrorsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000501-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ErrorsDisp = dispinterface
    ['{00000501-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Error readonly dispid 0; default;
    procedure Clear; dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Command15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command15 = interface(_ADO)
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: _Connection; safecall;
    procedure _Set_ActiveConnection(const ppvObject: _Connection); safecall;
    procedure Set_ActiveConnection(ppvObject: OleVariant); safecall;
    function Get_CommandText: WideString; safecall;
    procedure Set_CommandText(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(pl: Integer); safecall;
    function Get_Prepared: WordBool; safecall;
    procedure Set_Prepared(pfPrepared: WordBool); safecall;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; safecall;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; safecall;
    function Get_Parameters: Parameters; safecall;
    procedure Set_CommandType(plCmdType: CommandTypeEnum); safecall;
    function Get_CommandType: CommandTypeEnum; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstrName: WideString); safecall;
    property CommandText: WideString read Get_CommandText write Set_CommandText;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property Prepared: WordBool read Get_Prepared write Set_Prepared;
    property Parameters: Parameters read Get_Parameters;
    property CommandType: CommandTypeEnum read Get_CommandType write Set_CommandType;
    property Name: WideString read Get_Name write Set_Name;
  end;

// *********************************************************************//
// DispIntf:  Command15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000508-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command15Disp = dispinterface
    ['{00000508-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Connection15
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Connection15 = interface(_ADO)
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    function Get_ConnectionString: WideString; safecall;
    procedure Set_ConnectionString(const pbstr: WideString); safecall;
    function Get_CommandTimeout: Integer; safecall;
    procedure Set_CommandTimeout(plTimeout: Integer); safecall;
    function Get_ConnectionTimeout: Integer; safecall;
    procedure Set_ConnectionTimeout(plTimeout: Integer); safecall;
    function Get_Version: WideString; safecall;
    procedure Close; safecall;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; safecall;
    function BeginTrans: Integer; safecall;
    procedure CommitTrans; safecall;
    procedure RollbackTrans; safecall;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); safecall;
    function Get_Errors: Errors; safecall;
    function Get_DefaultDatabase: WideString; safecall;
    procedure Set_DefaultDatabase(const pbstr: WideString); safecall;
    function Get_IsolationLevel: IsolationLevelEnum; safecall;
    procedure Set_IsolationLevel(Level: IsolationLevelEnum); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plAttr: Integer); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(plMode: ConnectModeEnum); safecall;
    function Get_Provider: WideString; safecall;
    procedure Set_Provider(const pbstr: WideString); safecall;
    function Get_State: Integer; safecall;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; safecall;
    property ConnectionString: WideString read Get_ConnectionString write Set_ConnectionString;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property ConnectionTimeout: Integer read Get_ConnectionTimeout write Set_ConnectionTimeout;
    property Version: WideString read Get_Version;
    property Errors: Errors read Get_Errors;
    property DefaultDatabase: WideString read Get_DefaultDatabase write Set_DefaultDatabase;
    property IsolationLevel: IsolationLevelEnum read Get_IsolationLevel write Set_IsolationLevel;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Provider: WideString read Get_Provider write Set_Provider;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  Connection15Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000515-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Connection15Disp = dispinterface
    ['{00000515-0000-0010-8000-00AA006D2EA4}']
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: Errors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; dispid 19;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Connection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Connection = interface(Connection15)
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod__Connection_0_1; safecall;
    procedure GhostMethod__Connection_4_2; safecall;
    procedure GhostMethod__Connection_8_3; safecall;
    procedure GhostMethod__Connection_12_4; safecall;
    procedure GhostMethod__Connection_16_5; safecall;
    procedure GhostMethod__Connection_20_6; safecall;
    procedure GhostMethod__Connection_24_7; safecall;
    procedure GhostMethod__Connection_28_8; safecall;
    procedure GhostMethod__Connection_32_9; safecall;
    procedure GhostMethod__Connection_36_10; safecall;
    procedure GhostMethod__Connection_40_11; safecall;
    procedure GhostMethod__Connection_44_12; safecall;
    procedure GhostMethod__Connection_48_13; safecall;
    procedure GhostMethod__Connection_52_14; safecall;
    procedure GhostMethod__Connection_56_15; safecall;
    procedure GhostMethod__Connection_60_16; safecall;
    procedure GhostMethod__Connection_64_17; safecall;
    procedure GhostMethod__Connection_68_18; safecall;
    procedure GhostMethod__Connection_72_19; safecall;
    procedure GhostMethod__Connection_76_20; safecall;
    procedure GhostMethod__Connection_80_21; safecall;
    procedure GhostMethod__Connection_84_22; safecall;
    procedure GhostMethod__Connection_88_23; safecall;
    procedure GhostMethod__Connection_92_24; safecall;
    procedure GhostMethod__Connection_96_25; safecall;
    procedure GhostMethod__Connection_100_26; safecall;
    procedure GhostMethod__Connection_104_27; safecall;
    procedure GhostMethod__Connection_108_28; safecall;
    procedure GhostMethod__Connection_112_29; safecall;
    procedure GhostMethod__Connection_116_30; safecall;
    procedure GhostMethod__Connection_120_31; safecall;
    procedure GhostMethod__Connection_124_32; safecall;
    procedure GhostMethod__Connection_128_33; safecall;
    procedure GhostMethod__Connection_132_34; safecall;
    procedure GhostMethod__Connection_136_35; safecall;
    procedure GhostMethod__Connection_140_36; safecall;
    procedure Cancel; safecall;
  end;

// *********************************************************************//
// DispIntf:  _ConnectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00000550-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ConnectionDisp = dispinterface
    ['{00000550-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod__Connection_0_1; dispid 1610678272;
    procedure GhostMethod__Connection_4_2; dispid 1610678273;
    procedure GhostMethod__Connection_8_3; dispid 1610678274;
    procedure GhostMethod__Connection_12_4; dispid 1610678275;
    procedure GhostMethod__Connection_16_5; dispid 1610678276;
    procedure GhostMethod__Connection_20_6; dispid 1610678277;
    procedure GhostMethod__Connection_24_7; dispid 1610678278;
    procedure GhostMethod__Connection_28_8; dispid 1610678279;
    procedure GhostMethod__Connection_32_9; dispid 1610678280;
    procedure GhostMethod__Connection_36_10; dispid 1610678281;
    procedure GhostMethod__Connection_40_11; dispid 1610678282;
    procedure GhostMethod__Connection_44_12; dispid 1610678283;
    procedure GhostMethod__Connection_48_13; dispid 1610678284;
    procedure GhostMethod__Connection_52_14; dispid 1610678285;
    procedure GhostMethod__Connection_56_15; dispid 1610678286;
    procedure GhostMethod__Connection_60_16; dispid 1610678287;
    procedure GhostMethod__Connection_64_17; dispid 1610678288;
    procedure GhostMethod__Connection_68_18; dispid 1610678289;
    procedure GhostMethod__Connection_72_19; dispid 1610678290;
    procedure GhostMethod__Connection_76_20; dispid 1610678291;
    procedure GhostMethod__Connection_80_21; dispid 1610678292;
    procedure GhostMethod__Connection_84_22; dispid 1610678293;
    procedure GhostMethod__Connection_88_23; dispid 1610678294;
    procedure GhostMethod__Connection_92_24; dispid 1610678295;
    procedure GhostMethod__Connection_96_25; dispid 1610678296;
    procedure GhostMethod__Connection_100_26; dispid 1610678297;
    procedure GhostMethod__Connection_104_27; dispid 1610678298;
    procedure GhostMethod__Connection_108_28; dispid 1610678299;
    procedure GhostMethod__Connection_112_29; dispid 1610678300;
    procedure GhostMethod__Connection_116_30; dispid 1610678301;
    procedure GhostMethod__Connection_120_31; dispid 1610678302;
    procedure GhostMethod__Connection_124_32; dispid 1610678303;
    procedure GhostMethod__Connection_128_33; dispid 1610678304;
    procedure GhostMethod__Connection_132_34; dispid 1610678305;
    procedure GhostMethod__Connection_136_35; dispid 1610678306;
    procedure GhostMethod__Connection_140_36; dispid 1610678307;
    procedure Cancel; dispid 21;
    property ConnectionString: WideString dispid 0;
    property CommandTimeout: Integer dispid 2;
    property ConnectionTimeout: Integer dispid 3;
    property Version: WideString readonly dispid 4;
    procedure Close; dispid 5;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset; dispid 6;
    function BeginTrans: Integer; dispid 7;
    procedure CommitTrans; dispid 8;
    procedure RollbackTrans; dispid 9;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer); dispid 10;
    property Errors: Errors readonly dispid 11;
    property DefaultDatabase: WideString dispid 12;
    property IsolationLevel: IsolationLevelEnum dispid 13;
    property Attributes: Integer dispid 14;
    property CursorLocation: CursorLocationEnum dispid 15;
    property Mode: ConnectModeEnum dispid 16;
    property Provider: WideString dispid 17;
    property State: Integer readonly dispid 18;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; dispid 19;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset15 = interface(_ADO)
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    function Get_AbsolutePosition: PositionEnum_Param; safecall;
    procedure Set_AbsolutePosition(pl: PositionEnum_Param); safecall;
    procedure _Set_ActiveConnection(const pvar: IDispatch); safecall;
    procedure Set_ActiveConnection(pvar: OleVariant); safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    function Get_BOF: WordBool; safecall;
    function Get_Bookmark: OleVariant; safecall;
    procedure Set_Bookmark(pvBookmark: OleVariant); safecall;
    function Get_CacheSize: Integer; safecall;
    procedure Set_CacheSize(pl: Integer); safecall;
    function Get_CursorType: CursorTypeEnum; safecall;
    procedure Set_CursorType(plCursorType: CursorTypeEnum); safecall;
    function Get_EOF: WordBool; safecall;
    function Get_Fields: Fields; safecall;
    function Get_LockType: LockTypeEnum; safecall;
    procedure Set_LockType(plLockType: LockTypeEnum); safecall;
    function Get_MaxRecords: ADO_LONGPTR; safecall;
    procedure Set_MaxRecords(plMaxRecords: ADO_LONGPTR); safecall;
    function Get_RecordCount: ADO_LONGPTR; safecall;
    procedure _Set_Source(const pvSource: IDispatch); safecall;
    procedure Set_Source(const pvSource: WideString); safecall;
    function Get_Source: OleVariant; safecall;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); safecall;
    procedure CancelUpdate; safecall;
    procedure Close; safecall;
    procedure Delete(AffectRecords: AffectEnum); safecall;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; safecall;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); safecall;
    procedure MoveNext; safecall;
    procedure MovePrevious; safecall;
    procedure MoveFirst; safecall;
    procedure MoveLast; safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); safecall;
    procedure Requery(Options: Integer); safecall;
    procedure _xResync(AffectRecords: AffectEnum); safecall;
    procedure Update(Fields: OleVariant; Values: OleVariant); safecall;
    function Get_AbsolutePage: PositionEnum_Param; safecall;
    procedure Set_AbsolutePage(pl: PositionEnum_Param); safecall;
    function Get_EditMode: EditModeEnum; safecall;
    function Get_Filter: OleVariant; safecall;
    procedure Set_Filter(Criteria: OleVariant); safecall;
    function Get_PageCount: ADO_LONGPTR; safecall;
    function Get_PageSize: Integer; safecall;
    procedure Set_PageSize(pl: Integer); safecall;
    function Get_Sort: WideString; safecall;
    procedure Set_Sort(const Criteria: WideString); safecall;
    function Get_Status: Integer; safecall;
    function Get_State: Integer; safecall;
    function _xClone: _Recordset; safecall;
    procedure UpdateBatch(AffectRecords: AffectEnum); safecall;
    procedure CancelBatch(AffectRecords: AffectEnum); safecall;
    function Get_CursorLocation: CursorLocationEnum; safecall;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum); safecall;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; safecall;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; safecall;
    function Get_Collect(Index: OleVariant): OleVariant; safecall;
    procedure Set_Collect(Index: OleVariant; pvar: OleVariant); safecall;
    function Get_MarshalOptions: MarshalOptionsEnum; safecall;
    procedure Set_MarshalOptions(peMarshal: MarshalOptionsEnum); safecall;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); safecall;
    property AbsolutePosition: PositionEnum_Param read Get_AbsolutePosition write Set_AbsolutePosition;
    property BOF: WordBool read Get_BOF;
    property Bookmark: OleVariant read Get_Bookmark write Set_Bookmark;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize;
    property CursorType: CursorTypeEnum read Get_CursorType write Set_CursorType;
    property EOF: WordBool read Get_EOF;
    property Fields: Fields read Get_Fields;
    property LockType: LockTypeEnum read Get_LockType write Set_LockType;
    property MaxRecords: ADO_LONGPTR read Get_MaxRecords write Set_MaxRecords;
    property RecordCount: ADO_LONGPTR read Get_RecordCount;
    property AbsolutePage: PositionEnum_Param read Get_AbsolutePage write Set_AbsolutePage;
    property EditMode: EditModeEnum read Get_EditMode;
    property Filter: OleVariant read Get_Filter write Set_Filter;
    property PageCount: ADO_LONGPTR read Get_PageCount;
    property PageSize: Integer read Get_PageSize write Set_PageSize;
    property Sort: WideString read Get_Sort write Set_Sort;
    property Status: Integer read Get_Status;
    property State: Integer read Get_State;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Collect[Index: OleVariant]: OleVariant read Get_Collect write Set_Collect;
    property MarshalOptions: MarshalOptionsEnum read Get_MarshalOptions write Set_MarshalOptions;
  end;

// *********************************************************************//
// DispIntf:  Recordset15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset15Disp = dispinterface
    ['{0000050E-0000-0010-8000-00AA006D2EA4}']
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset20 = interface(Recordset15)
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Recordset20_0_1; safecall;
    procedure GhostMethod_Recordset20_4_2; safecall;
    procedure GhostMethod_Recordset20_8_3; safecall;
    procedure GhostMethod_Recordset20_12_4; safecall;
    procedure GhostMethod_Recordset20_16_5; safecall;
    procedure GhostMethod_Recordset20_20_6; safecall;
    procedure GhostMethod_Recordset20_24_7; safecall;
    procedure GhostMethod_Recordset20_28_8; safecall;
    procedure GhostMethod_Recordset20_32_9; safecall;
    procedure GhostMethod_Recordset20_36_10; safecall;
    procedure GhostMethod_Recordset20_40_11; safecall;
    procedure GhostMethod_Recordset20_44_12; safecall;
    procedure GhostMethod_Recordset20_48_13; safecall;
    procedure GhostMethod_Recordset20_52_14; safecall;
    procedure GhostMethod_Recordset20_56_15; safecall;
    procedure GhostMethod_Recordset20_60_16; safecall;
    procedure GhostMethod_Recordset20_64_17; safecall;
    procedure GhostMethod_Recordset20_68_18; safecall;
    procedure GhostMethod_Recordset20_72_19; safecall;
    procedure GhostMethod_Recordset20_76_20; safecall;
    procedure GhostMethod_Recordset20_80_21; safecall;
    procedure GhostMethod_Recordset20_84_22; safecall;
    procedure GhostMethod_Recordset20_88_23; safecall;
    procedure GhostMethod_Recordset20_92_24; safecall;
    procedure GhostMethod_Recordset20_96_25; safecall;
    procedure GhostMethod_Recordset20_100_26; safecall;
    procedure GhostMethod_Recordset20_104_27; safecall;
    procedure GhostMethod_Recordset20_108_28; safecall;
    procedure GhostMethod_Recordset20_112_29; safecall;
    procedure GhostMethod_Recordset20_116_30; safecall;
    procedure GhostMethod_Recordset20_120_31; safecall;
    procedure GhostMethod_Recordset20_124_32; safecall;
    procedure GhostMethod_Recordset20_128_33; safecall;
    procedure GhostMethod_Recordset20_132_34; safecall;
    procedure GhostMethod_Recordset20_136_35; safecall;
    procedure GhostMethod_Recordset20_140_36; safecall;
    procedure GhostMethod_Recordset20_144_37; safecall;
    procedure GhostMethod_Recordset20_148_38; safecall;
    procedure GhostMethod_Recordset20_152_39; safecall;
    procedure GhostMethod_Recordset20_156_40; safecall;
    procedure GhostMethod_Recordset20_160_41; safecall;
    procedure GhostMethod_Recordset20_164_42; safecall;
    procedure GhostMethod_Recordset20_168_43; safecall;
    procedure GhostMethod_Recordset20_172_44; safecall;
    procedure GhostMethod_Recordset20_176_45; safecall;
    procedure GhostMethod_Recordset20_180_46; safecall;
    procedure GhostMethod_Recordset20_184_47; safecall;
    procedure GhostMethod_Recordset20_188_48; safecall;
    procedure GhostMethod_Recordset20_192_49; safecall;
    procedure GhostMethod_Recordset20_196_50; safecall;
    procedure GhostMethod_Recordset20_200_51; safecall;
    procedure GhostMethod_Recordset20_204_52; safecall;
    procedure GhostMethod_Recordset20_208_53; safecall;
    procedure GhostMethod_Recordset20_212_54; safecall;
    procedure GhostMethod_Recordset20_216_55; safecall;
    procedure GhostMethod_Recordset20_220_56; safecall;
    procedure GhostMethod_Recordset20_224_57; safecall;
    procedure GhostMethod_Recordset20_228_58; safecall;
    procedure GhostMethod_Recordset20_232_59; safecall;
    procedure GhostMethod_Recordset20_236_60; safecall;
    procedure GhostMethod_Recordset20_240_61; safecall;
    procedure GhostMethod_Recordset20_244_62; safecall;
    procedure GhostMethod_Recordset20_248_63; safecall;
    procedure GhostMethod_Recordset20_252_64; safecall;
    procedure GhostMethod_Recordset20_256_65; safecall;
    procedure GhostMethod_Recordset20_260_66; safecall;
    procedure GhostMethod_Recordset20_264_67; safecall;
    procedure GhostMethod_Recordset20_268_68; safecall;
    procedure Cancel; safecall;
    function Get_DataSource: IUnknown; safecall;
    procedure _Set_DataSource(const ppunkDataSource: IUnknown); safecall;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); safecall;
    function Get_ActiveCommand: IDispatch; safecall;
    procedure Set_StayInSync(pbStayInSync: WordBool); safecall;
    function Get_StayInSync: WordBool; safecall;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; safecall;
    function Get_DataMember: WideString; safecall;
    procedure Set_DataMember(const pbstrDataMember: WideString); safecall;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; safecall;
    function Clone(LockType: LockTypeEnum): _Recordset; safecall;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); safecall;
    property DataSource: IUnknown read Get_DataSource write _Set_DataSource;
    property ActiveCommand: IDispatch read Get_ActiveCommand;
    property StayInSync: WordBool read Get_StayInSync write Set_StayInSync;
    property DataMember: WideString read Get_DataMember write Set_DataMember;
  end;

// *********************************************************************//
// DispIntf:  Recordset20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset20Disp = dispinterface
    ['{0000054F-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Recordset20_0_1; dispid 1610678272;
    procedure GhostMethod_Recordset20_4_2; dispid 1610678273;
    procedure GhostMethod_Recordset20_8_3; dispid 1610678274;
    procedure GhostMethod_Recordset20_12_4; dispid 1610678275;
    procedure GhostMethod_Recordset20_16_5; dispid 1610678276;
    procedure GhostMethod_Recordset20_20_6; dispid 1610678277;
    procedure GhostMethod_Recordset20_24_7; dispid 1610678278;
    procedure GhostMethod_Recordset20_28_8; dispid 1610678279;
    procedure GhostMethod_Recordset20_32_9; dispid 1610678280;
    procedure GhostMethod_Recordset20_36_10; dispid 1610678281;
    procedure GhostMethod_Recordset20_40_11; dispid 1610678282;
    procedure GhostMethod_Recordset20_44_12; dispid 1610678283;
    procedure GhostMethod_Recordset20_48_13; dispid 1610678284;
    procedure GhostMethod_Recordset20_52_14; dispid 1610678285;
    procedure GhostMethod_Recordset20_56_15; dispid 1610678286;
    procedure GhostMethod_Recordset20_60_16; dispid 1610678287;
    procedure GhostMethod_Recordset20_64_17; dispid 1610678288;
    procedure GhostMethod_Recordset20_68_18; dispid 1610678289;
    procedure GhostMethod_Recordset20_72_19; dispid 1610678290;
    procedure GhostMethod_Recordset20_76_20; dispid 1610678291;
    procedure GhostMethod_Recordset20_80_21; dispid 1610678292;
    procedure GhostMethod_Recordset20_84_22; dispid 1610678293;
    procedure GhostMethod_Recordset20_88_23; dispid 1610678294;
    procedure GhostMethod_Recordset20_92_24; dispid 1610678295;
    procedure GhostMethod_Recordset20_96_25; dispid 1610678296;
    procedure GhostMethod_Recordset20_100_26; dispid 1610678297;
    procedure GhostMethod_Recordset20_104_27; dispid 1610678298;
    procedure GhostMethod_Recordset20_108_28; dispid 1610678299;
    procedure GhostMethod_Recordset20_112_29; dispid 1610678300;
    procedure GhostMethod_Recordset20_116_30; dispid 1610678301;
    procedure GhostMethod_Recordset20_120_31; dispid 1610678302;
    procedure GhostMethod_Recordset20_124_32; dispid 1610678303;
    procedure GhostMethod_Recordset20_128_33; dispid 1610678304;
    procedure GhostMethod_Recordset20_132_34; dispid 1610678305;
    procedure GhostMethod_Recordset20_136_35; dispid 1610678306;
    procedure GhostMethod_Recordset20_140_36; dispid 1610678307;
    procedure GhostMethod_Recordset20_144_37; dispid 1610678308;
    procedure GhostMethod_Recordset20_148_38; dispid 1610678309;
    procedure GhostMethod_Recordset20_152_39; dispid 1610678310;
    procedure GhostMethod_Recordset20_156_40; dispid 1610678311;
    procedure GhostMethod_Recordset20_160_41; dispid 1610678312;
    procedure GhostMethod_Recordset20_164_42; dispid 1610678313;
    procedure GhostMethod_Recordset20_168_43; dispid 1610678314;
    procedure GhostMethod_Recordset20_172_44; dispid 1610678315;
    procedure GhostMethod_Recordset20_176_45; dispid 1610678316;
    procedure GhostMethod_Recordset20_180_46; dispid 1610678317;
    procedure GhostMethod_Recordset20_184_47; dispid 1610678318;
    procedure GhostMethod_Recordset20_188_48; dispid 1610678319;
    procedure GhostMethod_Recordset20_192_49; dispid 1610678320;
    procedure GhostMethod_Recordset20_196_50; dispid 1610678321;
    procedure GhostMethod_Recordset20_200_51; dispid 1610678322;
    procedure GhostMethod_Recordset20_204_52; dispid 1610678323;
    procedure GhostMethod_Recordset20_208_53; dispid 1610678324;
    procedure GhostMethod_Recordset20_212_54; dispid 1610678325;
    procedure GhostMethod_Recordset20_216_55; dispid 1610678326;
    procedure GhostMethod_Recordset20_220_56; dispid 1610678327;
    procedure GhostMethod_Recordset20_224_57; dispid 1610678328;
    procedure GhostMethod_Recordset20_228_58; dispid 1610678329;
    procedure GhostMethod_Recordset20_232_59; dispid 1610678330;
    procedure GhostMethod_Recordset20_236_60; dispid 1610678331;
    procedure GhostMethod_Recordset20_240_61; dispid 1610678332;
    procedure GhostMethod_Recordset20_244_62; dispid 1610678333;
    procedure GhostMethod_Recordset20_248_63; dispid 1610678334;
    procedure GhostMethod_Recordset20_252_64; dispid 1610678335;
    procedure GhostMethod_Recordset20_256_65; dispid 1610678336;
    procedure GhostMethod_Recordset20_260_66; dispid 1610678337;
    procedure GhostMethod_Recordset20_264_67; dispid 1610678338;
    procedure GhostMethod_Recordset20_268_68; dispid 1610678339;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Recordset21
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset21 = interface(Recordset20)
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Recordset21_0_1; safecall;
    procedure GhostMethod_Recordset21_4_2; safecall;
    procedure GhostMethod_Recordset21_8_3; safecall;
    procedure GhostMethod_Recordset21_12_4; safecall;
    procedure GhostMethod_Recordset21_16_5; safecall;
    procedure GhostMethod_Recordset21_20_6; safecall;
    procedure GhostMethod_Recordset21_24_7; safecall;
    procedure GhostMethod_Recordset21_28_8; safecall;
    procedure GhostMethod_Recordset21_32_9; safecall;
    procedure GhostMethod_Recordset21_36_10; safecall;
    procedure GhostMethod_Recordset21_40_11; safecall;
    procedure GhostMethod_Recordset21_44_12; safecall;
    procedure GhostMethod_Recordset21_48_13; safecall;
    procedure GhostMethod_Recordset21_52_14; safecall;
    procedure GhostMethod_Recordset21_56_15; safecall;
    procedure GhostMethod_Recordset21_60_16; safecall;
    procedure GhostMethod_Recordset21_64_17; safecall;
    procedure GhostMethod_Recordset21_68_18; safecall;
    procedure GhostMethod_Recordset21_72_19; safecall;
    procedure GhostMethod_Recordset21_76_20; safecall;
    procedure GhostMethod_Recordset21_80_21; safecall;
    procedure GhostMethod_Recordset21_84_22; safecall;
    procedure GhostMethod_Recordset21_88_23; safecall;
    procedure GhostMethod_Recordset21_92_24; safecall;
    procedure GhostMethod_Recordset21_96_25; safecall;
    procedure GhostMethod_Recordset21_100_26; safecall;
    procedure GhostMethod_Recordset21_104_27; safecall;
    procedure GhostMethod_Recordset21_108_28; safecall;
    procedure GhostMethod_Recordset21_112_29; safecall;
    procedure GhostMethod_Recordset21_116_30; safecall;
    procedure GhostMethod_Recordset21_120_31; safecall;
    procedure GhostMethod_Recordset21_124_32; safecall;
    procedure GhostMethod_Recordset21_128_33; safecall;
    procedure GhostMethod_Recordset21_132_34; safecall;
    procedure GhostMethod_Recordset21_136_35; safecall;
    procedure GhostMethod_Recordset21_140_36; safecall;
    procedure GhostMethod_Recordset21_144_37; safecall;
    procedure GhostMethod_Recordset21_148_38; safecall;
    procedure GhostMethod_Recordset21_152_39; safecall;
    procedure GhostMethod_Recordset21_156_40; safecall;
    procedure GhostMethod_Recordset21_160_41; safecall;
    procedure GhostMethod_Recordset21_164_42; safecall;
    procedure GhostMethod_Recordset21_168_43; safecall;
    procedure GhostMethod_Recordset21_172_44; safecall;
    procedure GhostMethod_Recordset21_176_45; safecall;
    procedure GhostMethod_Recordset21_180_46; safecall;
    procedure GhostMethod_Recordset21_184_47; safecall;
    procedure GhostMethod_Recordset21_188_48; safecall;
    procedure GhostMethod_Recordset21_192_49; safecall;
    procedure GhostMethod_Recordset21_196_50; safecall;
    procedure GhostMethod_Recordset21_200_51; safecall;
    procedure GhostMethod_Recordset21_204_52; safecall;
    procedure GhostMethod_Recordset21_208_53; safecall;
    procedure GhostMethod_Recordset21_212_54; safecall;
    procedure GhostMethod_Recordset21_216_55; safecall;
    procedure GhostMethod_Recordset21_220_56; safecall;
    procedure GhostMethod_Recordset21_224_57; safecall;
    procedure GhostMethod_Recordset21_228_58; safecall;
    procedure GhostMethod_Recordset21_232_59; safecall;
    procedure GhostMethod_Recordset21_236_60; safecall;
    procedure GhostMethod_Recordset21_240_61; safecall;
    procedure GhostMethod_Recordset21_244_62; safecall;
    procedure GhostMethod_Recordset21_248_63; safecall;
    procedure GhostMethod_Recordset21_252_64; safecall;
    procedure GhostMethod_Recordset21_256_65; safecall;
    procedure GhostMethod_Recordset21_260_66; safecall;
    procedure GhostMethod_Recordset21_264_67; safecall;
    procedure GhostMethod_Recordset21_268_68; safecall;
    procedure GhostMethod_Recordset21_272_69; safecall;
    procedure GhostMethod_Recordset21_276_70; safecall;
    procedure GhostMethod_Recordset21_280_71; safecall;
    procedure GhostMethod_Recordset21_284_72; safecall;
    procedure GhostMethod_Recordset21_288_73; safecall;
    procedure GhostMethod_Recordset21_292_74; safecall;
    procedure GhostMethod_Recordset21_296_75; safecall;
    procedure GhostMethod_Recordset21_300_76; safecall;
    procedure GhostMethod_Recordset21_304_77; safecall;
    procedure GhostMethod_Recordset21_308_78; safecall;
    procedure GhostMethod_Recordset21_312_79; safecall;
    procedure GhostMethod_Recordset21_316_80; safecall;
    procedure GhostMethod_Recordset21_320_81; safecall;
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); safecall;
    procedure Set_Index(const pbstrIndex: WideString); safecall;
    function Get_Index: WideString; safecall;
    property Index: WideString read Get_Index write Set_Index;
  end;

// *********************************************************************//
// DispIntf:  Recordset21Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000555-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Recordset21Disp = dispinterface
    ['{00000555-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Recordset21_0_1; dispid 1610678272;
    procedure GhostMethod_Recordset21_4_2; dispid 1610678273;
    procedure GhostMethod_Recordset21_8_3; dispid 1610678274;
    procedure GhostMethod_Recordset21_12_4; dispid 1610678275;
    procedure GhostMethod_Recordset21_16_5; dispid 1610678276;
    procedure GhostMethod_Recordset21_20_6; dispid 1610678277;
    procedure GhostMethod_Recordset21_24_7; dispid 1610678278;
    procedure GhostMethod_Recordset21_28_8; dispid 1610678279;
    procedure GhostMethod_Recordset21_32_9; dispid 1610678280;
    procedure GhostMethod_Recordset21_36_10; dispid 1610678281;
    procedure GhostMethod_Recordset21_40_11; dispid 1610678282;
    procedure GhostMethod_Recordset21_44_12; dispid 1610678283;
    procedure GhostMethod_Recordset21_48_13; dispid 1610678284;
    procedure GhostMethod_Recordset21_52_14; dispid 1610678285;
    procedure GhostMethod_Recordset21_56_15; dispid 1610678286;
    procedure GhostMethod_Recordset21_60_16; dispid 1610678287;
    procedure GhostMethod_Recordset21_64_17; dispid 1610678288;
    procedure GhostMethod_Recordset21_68_18; dispid 1610678289;
    procedure GhostMethod_Recordset21_72_19; dispid 1610678290;
    procedure GhostMethod_Recordset21_76_20; dispid 1610678291;
    procedure GhostMethod_Recordset21_80_21; dispid 1610678292;
    procedure GhostMethod_Recordset21_84_22; dispid 1610678293;
    procedure GhostMethod_Recordset21_88_23; dispid 1610678294;
    procedure GhostMethod_Recordset21_92_24; dispid 1610678295;
    procedure GhostMethod_Recordset21_96_25; dispid 1610678296;
    procedure GhostMethod_Recordset21_100_26; dispid 1610678297;
    procedure GhostMethod_Recordset21_104_27; dispid 1610678298;
    procedure GhostMethod_Recordset21_108_28; dispid 1610678299;
    procedure GhostMethod_Recordset21_112_29; dispid 1610678300;
    procedure GhostMethod_Recordset21_116_30; dispid 1610678301;
    procedure GhostMethod_Recordset21_120_31; dispid 1610678302;
    procedure GhostMethod_Recordset21_124_32; dispid 1610678303;
    procedure GhostMethod_Recordset21_128_33; dispid 1610678304;
    procedure GhostMethod_Recordset21_132_34; dispid 1610678305;
    procedure GhostMethod_Recordset21_136_35; dispid 1610678306;
    procedure GhostMethod_Recordset21_140_36; dispid 1610678307;
    procedure GhostMethod_Recordset21_144_37; dispid 1610678308;
    procedure GhostMethod_Recordset21_148_38; dispid 1610678309;
    procedure GhostMethod_Recordset21_152_39; dispid 1610678310;
    procedure GhostMethod_Recordset21_156_40; dispid 1610678311;
    procedure GhostMethod_Recordset21_160_41; dispid 1610678312;
    procedure GhostMethod_Recordset21_164_42; dispid 1610678313;
    procedure GhostMethod_Recordset21_168_43; dispid 1610678314;
    procedure GhostMethod_Recordset21_172_44; dispid 1610678315;
    procedure GhostMethod_Recordset21_176_45; dispid 1610678316;
    procedure GhostMethod_Recordset21_180_46; dispid 1610678317;
    procedure GhostMethod_Recordset21_184_47; dispid 1610678318;
    procedure GhostMethod_Recordset21_188_48; dispid 1610678319;
    procedure GhostMethod_Recordset21_192_49; dispid 1610678320;
    procedure GhostMethod_Recordset21_196_50; dispid 1610678321;
    procedure GhostMethod_Recordset21_200_51; dispid 1610678322;
    procedure GhostMethod_Recordset21_204_52; dispid 1610678323;
    procedure GhostMethod_Recordset21_208_53; dispid 1610678324;
    procedure GhostMethod_Recordset21_212_54; dispid 1610678325;
    procedure GhostMethod_Recordset21_216_55; dispid 1610678326;
    procedure GhostMethod_Recordset21_220_56; dispid 1610678327;
    procedure GhostMethod_Recordset21_224_57; dispid 1610678328;
    procedure GhostMethod_Recordset21_228_58; dispid 1610678329;
    procedure GhostMethod_Recordset21_232_59; dispid 1610678330;
    procedure GhostMethod_Recordset21_236_60; dispid 1610678331;
    procedure GhostMethod_Recordset21_240_61; dispid 1610678332;
    procedure GhostMethod_Recordset21_244_62; dispid 1610678333;
    procedure GhostMethod_Recordset21_248_63; dispid 1610678334;
    procedure GhostMethod_Recordset21_252_64; dispid 1610678335;
    procedure GhostMethod_Recordset21_256_65; dispid 1610678336;
    procedure GhostMethod_Recordset21_260_66; dispid 1610678337;
    procedure GhostMethod_Recordset21_264_67; dispid 1610678338;
    procedure GhostMethod_Recordset21_268_68; dispid 1610678339;
    procedure GhostMethod_Recordset21_272_69; dispid 1610678340;
    procedure GhostMethod_Recordset21_276_70; dispid 1610678341;
    procedure GhostMethod_Recordset21_280_71; dispid 1610678342;
    procedure GhostMethod_Recordset21_284_72; dispid 1610678343;
    procedure GhostMethod_Recordset21_288_73; dispid 1610678344;
    procedure GhostMethod_Recordset21_292_74; dispid 1610678345;
    procedure GhostMethod_Recordset21_296_75; dispid 1610678346;
    procedure GhostMethod_Recordset21_300_76; dispid 1610678347;
    procedure GhostMethod_Recordset21_304_77; dispid 1610678348;
    procedure GhostMethod_Recordset21_308_78; dispid 1610678349;
    procedure GhostMethod_Recordset21_312_79; dispid 1610678350;
    procedure GhostMethod_Recordset21_316_80; dispid 1610678351;
    procedure GhostMethod_Recordset21_320_81; dispid 1610678352;
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure GhostMethod_Recordset20_0_1; dispid 1610678272;
    procedure GhostMethod_Recordset20_4_2; dispid 1610678273;
    procedure GhostMethod_Recordset20_8_3; dispid 1610678274;
    procedure GhostMethod_Recordset20_12_4; dispid 1610678275;
    procedure GhostMethod_Recordset20_16_5; dispid 1610678276;
    procedure GhostMethod_Recordset20_20_6; dispid 1610678277;
    procedure GhostMethod_Recordset20_24_7; dispid 1610678278;
    procedure GhostMethod_Recordset20_28_8; dispid 1610678279;
    procedure GhostMethod_Recordset20_32_9; dispid 1610678280;
    procedure GhostMethod_Recordset20_36_10; dispid 1610678281;
    procedure GhostMethod_Recordset20_40_11; dispid 1610678282;
    procedure GhostMethod_Recordset20_44_12; dispid 1610678283;
    procedure GhostMethod_Recordset20_48_13; dispid 1610678284;
    procedure GhostMethod_Recordset20_52_14; dispid 1610678285;
    procedure GhostMethod_Recordset20_56_15; dispid 1610678286;
    procedure GhostMethod_Recordset20_60_16; dispid 1610678287;
    procedure GhostMethod_Recordset20_64_17; dispid 1610678288;
    procedure GhostMethod_Recordset20_68_18; dispid 1610678289;
    procedure GhostMethod_Recordset20_72_19; dispid 1610678290;
    procedure GhostMethod_Recordset20_76_20; dispid 1610678291;
    procedure GhostMethod_Recordset20_80_21; dispid 1610678292;
    procedure GhostMethod_Recordset20_84_22; dispid 1610678293;
    procedure GhostMethod_Recordset20_88_23; dispid 1610678294;
    procedure GhostMethod_Recordset20_92_24; dispid 1610678295;
    procedure GhostMethod_Recordset20_96_25; dispid 1610678296;
    procedure GhostMethod_Recordset20_100_26; dispid 1610678297;
    procedure GhostMethod_Recordset20_104_27; dispid 1610678298;
    procedure GhostMethod_Recordset20_108_28; dispid 1610678299;
    procedure GhostMethod_Recordset20_112_29; dispid 1610678300;
    procedure GhostMethod_Recordset20_116_30; dispid 1610678301;
    procedure GhostMethod_Recordset20_120_31; dispid 1610678302;
    procedure GhostMethod_Recordset20_124_32; dispid 1610678303;
    procedure GhostMethod_Recordset20_128_33; dispid 1610678304;
    procedure GhostMethod_Recordset20_132_34; dispid 1610678305;
    procedure GhostMethod_Recordset20_136_35; dispid 1610678306;
    procedure GhostMethod_Recordset20_140_36; dispid 1610678307;
    procedure GhostMethod_Recordset20_144_37; dispid 1610678308;
    procedure GhostMethod_Recordset20_148_38; dispid 1610678309;
    procedure GhostMethod_Recordset20_152_39; dispid 1610678310;
    procedure GhostMethod_Recordset20_156_40; dispid 1610678311;
    procedure GhostMethod_Recordset20_160_41; dispid 1610678312;
    procedure GhostMethod_Recordset20_164_42; dispid 1610678313;
    procedure GhostMethod_Recordset20_168_43; dispid 1610678314;
    procedure GhostMethod_Recordset20_172_44; dispid 1610678315;
    procedure GhostMethod_Recordset20_176_45; dispid 1610678316;
    procedure GhostMethod_Recordset20_180_46; dispid 1610678317;
    procedure GhostMethod_Recordset20_184_47; dispid 1610678318;
    procedure GhostMethod_Recordset20_188_48; dispid 1610678319;
    procedure GhostMethod_Recordset20_192_49; dispid 1610678320;
    procedure GhostMethod_Recordset20_196_50; dispid 1610678321;
    procedure GhostMethod_Recordset20_200_51; dispid 1610678322;
    procedure GhostMethod_Recordset20_204_52; dispid 1610678323;
    procedure GhostMethod_Recordset20_208_53; dispid 1610678324;
    procedure GhostMethod_Recordset20_212_54; dispid 1610678325;
    procedure GhostMethod_Recordset20_216_55; dispid 1610678326;
    procedure GhostMethod_Recordset20_220_56; dispid 1610678327;
    procedure GhostMethod_Recordset20_224_57; dispid 1610678328;
    procedure GhostMethod_Recordset20_228_58; dispid 1610678329;
    procedure GhostMethod_Recordset20_232_59; dispid 1610678330;
    procedure GhostMethod_Recordset20_236_60; dispid 1610678331;
    procedure GhostMethod_Recordset20_240_61; dispid 1610678332;
    procedure GhostMethod_Recordset20_244_62; dispid 1610678333;
    procedure GhostMethod_Recordset20_248_63; dispid 1610678334;
    procedure GhostMethod_Recordset20_252_64; dispid 1610678335;
    procedure GhostMethod_Recordset20_256_65; dispid 1610678336;
    procedure GhostMethod_Recordset20_260_66; dispid 1610678337;
    procedure GhostMethod_Recordset20_264_67; dispid 1610678338;
    procedure GhostMethod_Recordset20_268_68; dispid 1610678339;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Recordset
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Recordset = interface(Recordset21)
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod__Recordset_0_1; safecall;
    procedure GhostMethod__Recordset_4_2; safecall;
    procedure GhostMethod__Recordset_8_3; safecall;
    procedure GhostMethod__Recordset_12_4; safecall;
    procedure GhostMethod__Recordset_16_5; safecall;
    procedure GhostMethod__Recordset_20_6; safecall;
    procedure GhostMethod__Recordset_24_7; safecall;
    procedure GhostMethod__Recordset_28_8; safecall;
    procedure GhostMethod__Recordset_32_9; safecall;
    procedure GhostMethod__Recordset_36_10; safecall;
    procedure GhostMethod__Recordset_40_11; safecall;
    procedure GhostMethod__Recordset_44_12; safecall;
    procedure GhostMethod__Recordset_48_13; safecall;
    procedure GhostMethod__Recordset_52_14; safecall;
    procedure GhostMethod__Recordset_56_15; safecall;
    procedure GhostMethod__Recordset_60_16; safecall;
    procedure GhostMethod__Recordset_64_17; safecall;
    procedure GhostMethod__Recordset_68_18; safecall;
    procedure GhostMethod__Recordset_72_19; safecall;
    procedure GhostMethod__Recordset_76_20; safecall;
    procedure GhostMethod__Recordset_80_21; safecall;
    procedure GhostMethod__Recordset_84_22; safecall;
    procedure GhostMethod__Recordset_88_23; safecall;
    procedure GhostMethod__Recordset_92_24; safecall;
    procedure GhostMethod__Recordset_96_25; safecall;
    procedure GhostMethod__Recordset_100_26; safecall;
    procedure GhostMethod__Recordset_104_27; safecall;
    procedure GhostMethod__Recordset_108_28; safecall;
    procedure GhostMethod__Recordset_112_29; safecall;
    procedure GhostMethod__Recordset_116_30; safecall;
    procedure GhostMethod__Recordset_120_31; safecall;
    procedure GhostMethod__Recordset_124_32; safecall;
    procedure GhostMethod__Recordset_128_33; safecall;
    procedure GhostMethod__Recordset_132_34; safecall;
    procedure GhostMethod__Recordset_136_35; safecall;
    procedure GhostMethod__Recordset_140_36; safecall;
    procedure GhostMethod__Recordset_144_37; safecall;
    procedure GhostMethod__Recordset_148_38; safecall;
    procedure GhostMethod__Recordset_152_39; safecall;
    procedure GhostMethod__Recordset_156_40; safecall;
    procedure GhostMethod__Recordset_160_41; safecall;
    procedure GhostMethod__Recordset_164_42; safecall;
    procedure GhostMethod__Recordset_168_43; safecall;
    procedure GhostMethod__Recordset_172_44; safecall;
    procedure GhostMethod__Recordset_176_45; safecall;
    procedure GhostMethod__Recordset_180_46; safecall;
    procedure GhostMethod__Recordset_184_47; safecall;
    procedure GhostMethod__Recordset_188_48; safecall;
    procedure GhostMethod__Recordset_192_49; safecall;
    procedure GhostMethod__Recordset_196_50; safecall;
    procedure GhostMethod__Recordset_200_51; safecall;
    procedure GhostMethod__Recordset_204_52; safecall;
    procedure GhostMethod__Recordset_208_53; safecall;
    procedure GhostMethod__Recordset_212_54; safecall;
    procedure GhostMethod__Recordset_216_55; safecall;
    procedure GhostMethod__Recordset_220_56; safecall;
    procedure GhostMethod__Recordset_224_57; safecall;
    procedure GhostMethod__Recordset_228_58; safecall;
    procedure GhostMethod__Recordset_232_59; safecall;
    procedure GhostMethod__Recordset_236_60; safecall;
    procedure GhostMethod__Recordset_240_61; safecall;
    procedure GhostMethod__Recordset_244_62; safecall;
    procedure GhostMethod__Recordset_248_63; safecall;
    procedure GhostMethod__Recordset_252_64; safecall;
    procedure GhostMethod__Recordset_256_65; safecall;
    procedure GhostMethod__Recordset_260_66; safecall;
    procedure GhostMethod__Recordset_264_67; safecall;
    procedure GhostMethod__Recordset_268_68; safecall;
    procedure GhostMethod__Recordset_272_69; safecall;
    procedure GhostMethod__Recordset_276_70; safecall;
    procedure GhostMethod__Recordset_280_71; safecall;
    procedure GhostMethod__Recordset_284_72; safecall;
    procedure GhostMethod__Recordset_288_73; safecall;
    procedure GhostMethod__Recordset_292_74; safecall;
    procedure GhostMethod__Recordset_296_75; safecall;
    procedure GhostMethod__Recordset_300_76; safecall;
    procedure GhostMethod__Recordset_304_77; safecall;
    procedure GhostMethod__Recordset_308_78; safecall;
    procedure GhostMethod__Recordset_312_79; safecall;
    procedure GhostMethod__Recordset_316_80; safecall;
    procedure GhostMethod__Recordset_320_81; safecall;
    procedure GhostMethod__Recordset_324_82; safecall;
    procedure GhostMethod__Recordset_328_83; safecall;
    procedure GhostMethod__Recordset_332_84; safecall;
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); safecall;
  end;

// *********************************************************************//
// DispIntf:  _RecordsetDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000556-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _RecordsetDisp = dispinterface
    ['{00000556-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod__Recordset_0_1; dispid 1610678272;
    procedure GhostMethod__Recordset_4_2; dispid 1610678273;
    procedure GhostMethod__Recordset_8_3; dispid 1610678274;
    procedure GhostMethod__Recordset_12_4; dispid 1610678275;
    procedure GhostMethod__Recordset_16_5; dispid 1610678276;
    procedure GhostMethod__Recordset_20_6; dispid 1610678277;
    procedure GhostMethod__Recordset_24_7; dispid 1610678278;
    procedure GhostMethod__Recordset_28_8; dispid 1610678279;
    procedure GhostMethod__Recordset_32_9; dispid 1610678280;
    procedure GhostMethod__Recordset_36_10; dispid 1610678281;
    procedure GhostMethod__Recordset_40_11; dispid 1610678282;
    procedure GhostMethod__Recordset_44_12; dispid 1610678283;
    procedure GhostMethod__Recordset_48_13; dispid 1610678284;
    procedure GhostMethod__Recordset_52_14; dispid 1610678285;
    procedure GhostMethod__Recordset_56_15; dispid 1610678286;
    procedure GhostMethod__Recordset_60_16; dispid 1610678287;
    procedure GhostMethod__Recordset_64_17; dispid 1610678288;
    procedure GhostMethod__Recordset_68_18; dispid 1610678289;
    procedure GhostMethod__Recordset_72_19; dispid 1610678290;
    procedure GhostMethod__Recordset_76_20; dispid 1610678291;
    procedure GhostMethod__Recordset_80_21; dispid 1610678292;
    procedure GhostMethod__Recordset_84_22; dispid 1610678293;
    procedure GhostMethod__Recordset_88_23; dispid 1610678294;
    procedure GhostMethod__Recordset_92_24; dispid 1610678295;
    procedure GhostMethod__Recordset_96_25; dispid 1610678296;
    procedure GhostMethod__Recordset_100_26; dispid 1610678297;
    procedure GhostMethod__Recordset_104_27; dispid 1610678298;
    procedure GhostMethod__Recordset_108_28; dispid 1610678299;
    procedure GhostMethod__Recordset_112_29; dispid 1610678300;
    procedure GhostMethod__Recordset_116_30; dispid 1610678301;
    procedure GhostMethod__Recordset_120_31; dispid 1610678302;
    procedure GhostMethod__Recordset_124_32; dispid 1610678303;
    procedure GhostMethod__Recordset_128_33; dispid 1610678304;
    procedure GhostMethod__Recordset_132_34; dispid 1610678305;
    procedure GhostMethod__Recordset_136_35; dispid 1610678306;
    procedure GhostMethod__Recordset_140_36; dispid 1610678307;
    procedure GhostMethod__Recordset_144_37; dispid 1610678308;
    procedure GhostMethod__Recordset_148_38; dispid 1610678309;
    procedure GhostMethod__Recordset_152_39; dispid 1610678310;
    procedure GhostMethod__Recordset_156_40; dispid 1610678311;
    procedure GhostMethod__Recordset_160_41; dispid 1610678312;
    procedure GhostMethod__Recordset_164_42; dispid 1610678313;
    procedure GhostMethod__Recordset_168_43; dispid 1610678314;
    procedure GhostMethod__Recordset_172_44; dispid 1610678315;
    procedure GhostMethod__Recordset_176_45; dispid 1610678316;
    procedure GhostMethod__Recordset_180_46; dispid 1610678317;
    procedure GhostMethod__Recordset_184_47; dispid 1610678318;
    procedure GhostMethod__Recordset_188_48; dispid 1610678319;
    procedure GhostMethod__Recordset_192_49; dispid 1610678320;
    procedure GhostMethod__Recordset_196_50; dispid 1610678321;
    procedure GhostMethod__Recordset_200_51; dispid 1610678322;
    procedure GhostMethod__Recordset_204_52; dispid 1610678323;
    procedure GhostMethod__Recordset_208_53; dispid 1610678324;
    procedure GhostMethod__Recordset_212_54; dispid 1610678325;
    procedure GhostMethod__Recordset_216_55; dispid 1610678326;
    procedure GhostMethod__Recordset_220_56; dispid 1610678327;
    procedure GhostMethod__Recordset_224_57; dispid 1610678328;
    procedure GhostMethod__Recordset_228_58; dispid 1610678329;
    procedure GhostMethod__Recordset_232_59; dispid 1610678330;
    procedure GhostMethod__Recordset_236_60; dispid 1610678331;
    procedure GhostMethod__Recordset_240_61; dispid 1610678332;
    procedure GhostMethod__Recordset_244_62; dispid 1610678333;
    procedure GhostMethod__Recordset_248_63; dispid 1610678334;
    procedure GhostMethod__Recordset_252_64; dispid 1610678335;
    procedure GhostMethod__Recordset_256_65; dispid 1610678336;
    procedure GhostMethod__Recordset_260_66; dispid 1610678337;
    procedure GhostMethod__Recordset_264_67; dispid 1610678338;
    procedure GhostMethod__Recordset_268_68; dispid 1610678339;
    procedure GhostMethod__Recordset_272_69; dispid 1610678340;
    procedure GhostMethod__Recordset_276_70; dispid 1610678341;
    procedure GhostMethod__Recordset_280_71; dispid 1610678342;
    procedure GhostMethod__Recordset_284_72; dispid 1610678343;
    procedure GhostMethod__Recordset_288_73; dispid 1610678344;
    procedure GhostMethod__Recordset_292_74; dispid 1610678345;
    procedure GhostMethod__Recordset_296_75; dispid 1610678346;
    procedure GhostMethod__Recordset_300_76; dispid 1610678347;
    procedure GhostMethod__Recordset_304_77; dispid 1610678348;
    procedure GhostMethod__Recordset_308_78; dispid 1610678349;
    procedure GhostMethod__Recordset_312_79; dispid 1610678350;
    procedure GhostMethod__Recordset_316_80; dispid 1610678351;
    procedure GhostMethod__Recordset_320_81; dispid 1610678352;
    procedure GhostMethod__Recordset_324_82; dispid 1610678353;
    procedure GhostMethod__Recordset_328_83; dispid 1610678354;
    procedure GhostMethod__Recordset_332_84; dispid 1610678355;
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum); dispid 1057;
    procedure GhostMethod_Recordset21_0_1; dispid 1610678272;
    procedure GhostMethod_Recordset21_4_2; dispid 1610678273;
    procedure GhostMethod_Recordset21_8_3; dispid 1610678274;
    procedure GhostMethod_Recordset21_12_4; dispid 1610678275;
    procedure GhostMethod_Recordset21_16_5; dispid 1610678276;
    procedure GhostMethod_Recordset21_20_6; dispid 1610678277;
    procedure GhostMethod_Recordset21_24_7; dispid 1610678278;
    procedure GhostMethod_Recordset21_28_8; dispid 1610678279;
    procedure GhostMethod_Recordset21_32_9; dispid 1610678280;
    procedure GhostMethod_Recordset21_36_10; dispid 1610678281;
    procedure GhostMethod_Recordset21_40_11; dispid 1610678282;
    procedure GhostMethod_Recordset21_44_12; dispid 1610678283;
    procedure GhostMethod_Recordset21_48_13; dispid 1610678284;
    procedure GhostMethod_Recordset21_52_14; dispid 1610678285;
    procedure GhostMethod_Recordset21_56_15; dispid 1610678286;
    procedure GhostMethod_Recordset21_60_16; dispid 1610678287;
    procedure GhostMethod_Recordset21_64_17; dispid 1610678288;
    procedure GhostMethod_Recordset21_68_18; dispid 1610678289;
    procedure GhostMethod_Recordset21_72_19; dispid 1610678290;
    procedure GhostMethod_Recordset21_76_20; dispid 1610678291;
    procedure GhostMethod_Recordset21_80_21; dispid 1610678292;
    procedure GhostMethod_Recordset21_84_22; dispid 1610678293;
    procedure GhostMethod_Recordset21_88_23; dispid 1610678294;
    procedure GhostMethod_Recordset21_92_24; dispid 1610678295;
    procedure GhostMethod_Recordset21_96_25; dispid 1610678296;
    procedure GhostMethod_Recordset21_100_26; dispid 1610678297;
    procedure GhostMethod_Recordset21_104_27; dispid 1610678298;
    procedure GhostMethod_Recordset21_108_28; dispid 1610678299;
    procedure GhostMethod_Recordset21_112_29; dispid 1610678300;
    procedure GhostMethod_Recordset21_116_30; dispid 1610678301;
    procedure GhostMethod_Recordset21_120_31; dispid 1610678302;
    procedure GhostMethod_Recordset21_124_32; dispid 1610678303;
    procedure GhostMethod_Recordset21_128_33; dispid 1610678304;
    procedure GhostMethod_Recordset21_132_34; dispid 1610678305;
    procedure GhostMethod_Recordset21_136_35; dispid 1610678306;
    procedure GhostMethod_Recordset21_140_36; dispid 1610678307;
    procedure GhostMethod_Recordset21_144_37; dispid 1610678308;
    procedure GhostMethod_Recordset21_148_38; dispid 1610678309;
    procedure GhostMethod_Recordset21_152_39; dispid 1610678310;
    procedure GhostMethod_Recordset21_156_40; dispid 1610678311;
    procedure GhostMethod_Recordset21_160_41; dispid 1610678312;
    procedure GhostMethod_Recordset21_164_42; dispid 1610678313;
    procedure GhostMethod_Recordset21_168_43; dispid 1610678314;
    procedure GhostMethod_Recordset21_172_44; dispid 1610678315;
    procedure GhostMethod_Recordset21_176_45; dispid 1610678316;
    procedure GhostMethod_Recordset21_180_46; dispid 1610678317;
    procedure GhostMethod_Recordset21_184_47; dispid 1610678318;
    procedure GhostMethod_Recordset21_188_48; dispid 1610678319;
    procedure GhostMethod_Recordset21_192_49; dispid 1610678320;
    procedure GhostMethod_Recordset21_196_50; dispid 1610678321;
    procedure GhostMethod_Recordset21_200_51; dispid 1610678322;
    procedure GhostMethod_Recordset21_204_52; dispid 1610678323;
    procedure GhostMethod_Recordset21_208_53; dispid 1610678324;
    procedure GhostMethod_Recordset21_212_54; dispid 1610678325;
    procedure GhostMethod_Recordset21_216_55; dispid 1610678326;
    procedure GhostMethod_Recordset21_220_56; dispid 1610678327;
    procedure GhostMethod_Recordset21_224_57; dispid 1610678328;
    procedure GhostMethod_Recordset21_228_58; dispid 1610678329;
    procedure GhostMethod_Recordset21_232_59; dispid 1610678330;
    procedure GhostMethod_Recordset21_236_60; dispid 1610678331;
    procedure GhostMethod_Recordset21_240_61; dispid 1610678332;
    procedure GhostMethod_Recordset21_244_62; dispid 1610678333;
    procedure GhostMethod_Recordset21_248_63; dispid 1610678334;
    procedure GhostMethod_Recordset21_252_64; dispid 1610678335;
    procedure GhostMethod_Recordset21_256_65; dispid 1610678336;
    procedure GhostMethod_Recordset21_260_66; dispid 1610678337;
    procedure GhostMethod_Recordset21_264_67; dispid 1610678338;
    procedure GhostMethod_Recordset21_268_68; dispid 1610678339;
    procedure GhostMethod_Recordset21_272_69; dispid 1610678340;
    procedure GhostMethod_Recordset21_276_70; dispid 1610678341;
    procedure GhostMethod_Recordset21_280_71; dispid 1610678342;
    procedure GhostMethod_Recordset21_284_72; dispid 1610678343;
    procedure GhostMethod_Recordset21_288_73; dispid 1610678344;
    procedure GhostMethod_Recordset21_292_74; dispid 1610678345;
    procedure GhostMethod_Recordset21_296_75; dispid 1610678346;
    procedure GhostMethod_Recordset21_300_76; dispid 1610678347;
    procedure GhostMethod_Recordset21_304_77; dispid 1610678348;
    procedure GhostMethod_Recordset21_308_78; dispid 1610678349;
    procedure GhostMethod_Recordset21_312_79; dispid 1610678350;
    procedure GhostMethod_Recordset21_316_80; dispid 1610678351;
    procedure GhostMethod_Recordset21_320_81; dispid 1610678352;
    procedure Seek(KeyValues: OleVariant; SeekOption: SeekEnum); dispid 1066;
    property Index: WideString dispid 1067;
    procedure GhostMethod_Recordset20_0_1; dispid 1610678272;
    procedure GhostMethod_Recordset20_4_2; dispid 1610678273;
    procedure GhostMethod_Recordset20_8_3; dispid 1610678274;
    procedure GhostMethod_Recordset20_12_4; dispid 1610678275;
    procedure GhostMethod_Recordset20_16_5; dispid 1610678276;
    procedure GhostMethod_Recordset20_20_6; dispid 1610678277;
    procedure GhostMethod_Recordset20_24_7; dispid 1610678278;
    procedure GhostMethod_Recordset20_28_8; dispid 1610678279;
    procedure GhostMethod_Recordset20_32_9; dispid 1610678280;
    procedure GhostMethod_Recordset20_36_10; dispid 1610678281;
    procedure GhostMethod_Recordset20_40_11; dispid 1610678282;
    procedure GhostMethod_Recordset20_44_12; dispid 1610678283;
    procedure GhostMethod_Recordset20_48_13; dispid 1610678284;
    procedure GhostMethod_Recordset20_52_14; dispid 1610678285;
    procedure GhostMethod_Recordset20_56_15; dispid 1610678286;
    procedure GhostMethod_Recordset20_60_16; dispid 1610678287;
    procedure GhostMethod_Recordset20_64_17; dispid 1610678288;
    procedure GhostMethod_Recordset20_68_18; dispid 1610678289;
    procedure GhostMethod_Recordset20_72_19; dispid 1610678290;
    procedure GhostMethod_Recordset20_76_20; dispid 1610678291;
    procedure GhostMethod_Recordset20_80_21; dispid 1610678292;
    procedure GhostMethod_Recordset20_84_22; dispid 1610678293;
    procedure GhostMethod_Recordset20_88_23; dispid 1610678294;
    procedure GhostMethod_Recordset20_92_24; dispid 1610678295;
    procedure GhostMethod_Recordset20_96_25; dispid 1610678296;
    procedure GhostMethod_Recordset20_100_26; dispid 1610678297;
    procedure GhostMethod_Recordset20_104_27; dispid 1610678298;
    procedure GhostMethod_Recordset20_108_28; dispid 1610678299;
    procedure GhostMethod_Recordset20_112_29; dispid 1610678300;
    procedure GhostMethod_Recordset20_116_30; dispid 1610678301;
    procedure GhostMethod_Recordset20_120_31; dispid 1610678302;
    procedure GhostMethod_Recordset20_124_32; dispid 1610678303;
    procedure GhostMethod_Recordset20_128_33; dispid 1610678304;
    procedure GhostMethod_Recordset20_132_34; dispid 1610678305;
    procedure GhostMethod_Recordset20_136_35; dispid 1610678306;
    procedure GhostMethod_Recordset20_140_36; dispid 1610678307;
    procedure GhostMethod_Recordset20_144_37; dispid 1610678308;
    procedure GhostMethod_Recordset20_148_38; dispid 1610678309;
    procedure GhostMethod_Recordset20_152_39; dispid 1610678310;
    procedure GhostMethod_Recordset20_156_40; dispid 1610678311;
    procedure GhostMethod_Recordset20_160_41; dispid 1610678312;
    procedure GhostMethod_Recordset20_164_42; dispid 1610678313;
    procedure GhostMethod_Recordset20_168_43; dispid 1610678314;
    procedure GhostMethod_Recordset20_172_44; dispid 1610678315;
    procedure GhostMethod_Recordset20_176_45; dispid 1610678316;
    procedure GhostMethod_Recordset20_180_46; dispid 1610678317;
    procedure GhostMethod_Recordset20_184_47; dispid 1610678318;
    procedure GhostMethod_Recordset20_188_48; dispid 1610678319;
    procedure GhostMethod_Recordset20_192_49; dispid 1610678320;
    procedure GhostMethod_Recordset20_196_50; dispid 1610678321;
    procedure GhostMethod_Recordset20_200_51; dispid 1610678322;
    procedure GhostMethod_Recordset20_204_52; dispid 1610678323;
    procedure GhostMethod_Recordset20_208_53; dispid 1610678324;
    procedure GhostMethod_Recordset20_212_54; dispid 1610678325;
    procedure GhostMethod_Recordset20_216_55; dispid 1610678326;
    procedure GhostMethod_Recordset20_220_56; dispid 1610678327;
    procedure GhostMethod_Recordset20_224_57; dispid 1610678328;
    procedure GhostMethod_Recordset20_228_58; dispid 1610678329;
    procedure GhostMethod_Recordset20_232_59; dispid 1610678330;
    procedure GhostMethod_Recordset20_236_60; dispid 1610678331;
    procedure GhostMethod_Recordset20_240_61; dispid 1610678332;
    procedure GhostMethod_Recordset20_244_62; dispid 1610678333;
    procedure GhostMethod_Recordset20_248_63; dispid 1610678334;
    procedure GhostMethod_Recordset20_252_64; dispid 1610678335;
    procedure GhostMethod_Recordset20_256_65; dispid 1610678336;
    procedure GhostMethod_Recordset20_260_66; dispid 1610678337;
    procedure GhostMethod_Recordset20_264_67; dispid 1610678338;
    procedure GhostMethod_Recordset20_268_68; dispid 1610678339;
    procedure Cancel; dispid 1055;
    property DataSource: IUnknown dispid 1056;
    procedure _xSave(const FileName: WideString; PersistFormat: PersistFormatEnum); dispid 1610874883;
    property ActiveCommand: IDispatch readonly dispid 1061;
    property StayInSync: WordBool dispid 1063;
    function GetString(StringFormat: StringFormatEnum; NumRows: Integer; 
                       const ColumnDelimeter: WideString; const RowDelimeter: WideString; 
                       const NullExpr: WideString): WideString; dispid 1062;
    property DataMember: WideString dispid 1064;
    function CompareBookmarks(Bookmark1: OleVariant; Bookmark2: OleVariant): CompareEnum; dispid 1065;
    function Clone(LockType: LockTypeEnum): _Recordset; dispid 1034;
    procedure Resync(AffectRecords: AffectEnum; ResyncValues: ResyncEnum); dispid 1024;
    property AbsolutePosition: PositionEnum_Param dispid 1000;
    function ActiveConnection: IDispatch; dispid 1001;
    property BOF: WordBool readonly dispid 1002;
    property Bookmark: OleVariant dispid 1003;
    property CacheSize: Integer dispid 1004;
    property CursorType: CursorTypeEnum dispid 1005;
    property EOF: WordBool readonly dispid 1006;
    property Fields: Fields readonly dispid 0;
    property LockType: LockTypeEnum dispid 1008;
    property MaxRecords: ADO_LONGPTR dispid 1009;
    property RecordCount: ADO_LONGPTR readonly dispid 1010;
    function Source: IDispatch; dispid 1011;
    procedure AddNew(FieldList: OleVariant; Values: OleVariant); dispid 1012;
    procedure CancelUpdate; dispid 1013;
    procedure Close; dispid 1014;
    procedure Delete(AffectRecords: AffectEnum); dispid 1015;
    function GetRows(Rows: Integer; Start: OleVariant; Fields: OleVariant): OleVariant; dispid 1016;
    procedure Move(NumRecords: ADO_LONGPTR; Start: OleVariant); dispid 1017;
    procedure MoveNext; dispid 1018;
    procedure MovePrevious; dispid 1019;
    procedure MoveFirst; dispid 1020;
    procedure MoveLast; dispid 1021;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; CursorType: CursorTypeEnum; 
                   LockType: LockTypeEnum; Options: Integer); dispid 1022;
    procedure Requery(Options: Integer); dispid 1023;
    procedure _xResync(AffectRecords: AffectEnum); dispid 1610809378;
    procedure Update(Fields: OleVariant; Values: OleVariant); dispid 1025;
    property AbsolutePage: PositionEnum_Param dispid 1047;
    property EditMode: EditModeEnum readonly dispid 1026;
    property Filter: OleVariant dispid 1030;
    property PageCount: ADO_LONGPTR readonly dispid 1050;
    property PageSize: Integer dispid 1048;
    property Sort: WideString dispid 1031;
    property Status: Integer readonly dispid 1029;
    property State: Integer readonly dispid 1054;
    function _xClone: _Recordset; dispid 1610809392;
    procedure UpdateBatch(AffectRecords: AffectEnum); dispid 1035;
    procedure CancelBatch(AffectRecords: AffectEnum); dispid 1049;
    property CursorLocation: CursorLocationEnum dispid 1051;
    function NextRecordset(out RecordsAffected: OleVariant): _Recordset; dispid 1052;
    function Supports(CursorOptions: CursorOptionEnum): WordBool; dispid 1036;
    property Collect[Index: OleVariant]: OleVariant dispid -8;
    property MarshalOptions: MarshalOptionsEnum dispid 1053;
    procedure Find(const Criteria: WideString; SkipRecords: ADO_LONGPTR; 
                   SearchDirection: SearchDirectionEnum; Start: OleVariant); dispid 1058;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Fields15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields15 = interface(_Collection)
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): Field; safecall;
    property Item[Index: OleVariant]: Field read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  Fields15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000506-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields15Disp = dispinterface
    ['{00000506-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Fields20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields20 = interface(Fields15)
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Fields20_0_1; safecall;
    procedure GhostMethod_Fields20_4_2; safecall;
    procedure GhostMethod_Fields20_8_3; safecall;
    procedure GhostMethod_Fields20_12_4; safecall;
    procedure GhostMethod_Fields20_16_5; safecall;
    procedure GhostMethod_Fields20_20_6; safecall;
    procedure GhostMethod_Fields20_24_7; safecall;
    procedure GhostMethod_Fields20_28_8; safecall;
    procedure GhostMethod_Fields20_32_9; safecall;
    procedure GhostMethod_Fields20_36_10; safecall;
    procedure GhostMethod_Fields20_40_11; safecall;
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); safecall;
    procedure Delete(Index: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  Fields20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields20Disp = dispinterface
    ['{0000054D-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Fields20_0_1; dispid 1610678272;
    procedure GhostMethod_Fields20_4_2; dispid 1610678273;
    procedure GhostMethod_Fields20_8_3; dispid 1610678274;
    procedure GhostMethod_Fields20_12_4; dispid 1610678275;
    procedure GhostMethod_Fields20_16_5; dispid 1610678276;
    procedure GhostMethod_Fields20_20_6; dispid 1610678277;
    procedure GhostMethod_Fields20_24_7; dispid 1610678278;
    procedure GhostMethod_Fields20_28_8; dispid 1610678279;
    procedure GhostMethod_Fields20_32_9; dispid 1610678280;
    procedure GhostMethod_Fields20_36_10; dispid 1610678281;
    procedure GhostMethod_Fields20_40_11; dispid 1610678282;
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Fields
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Fields = interface(Fields20)
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Fields_0_1; safecall;
    procedure GhostMethod_Fields_4_2; safecall;
    procedure GhostMethod_Fields_8_3; safecall;
    procedure GhostMethod_Fields_12_4; safecall;
    procedure GhostMethod_Fields_16_5; safecall;
    procedure GhostMethod_Fields_20_6; safecall;
    procedure GhostMethod_Fields_24_7; safecall;
    procedure GhostMethod_Fields_28_8; safecall;
    procedure GhostMethod_Fields_32_9; safecall;
    procedure GhostMethod_Fields_36_10; safecall;
    procedure GhostMethod_Fields_40_11; safecall;
    procedure GhostMethod_Fields_44_12; safecall;
    procedure GhostMethod_Fields_48_13; safecall;
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); safecall;
    procedure Update; safecall;
    procedure Resync(ResyncValues: ResyncEnum); safecall;
    procedure CancelUpdate; safecall;
  end;

// *********************************************************************//
// DispIntf:  FieldsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000564-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  FieldsDisp = dispinterface
    ['{00000564-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Fields_0_1; dispid 1610678272;
    procedure GhostMethod_Fields_4_2; dispid 1610678273;
    procedure GhostMethod_Fields_8_3; dispid 1610678274;
    procedure GhostMethod_Fields_12_4; dispid 1610678275;
    procedure GhostMethod_Fields_16_5; dispid 1610678276;
    procedure GhostMethod_Fields_20_6; dispid 1610678277;
    procedure GhostMethod_Fields_24_7; dispid 1610678278;
    procedure GhostMethod_Fields_28_8; dispid 1610678279;
    procedure GhostMethod_Fields_32_9; dispid 1610678280;
    procedure GhostMethod_Fields_36_10; dispid 1610678281;
    procedure GhostMethod_Fields_40_11; dispid 1610678282;
    procedure GhostMethod_Fields_44_12; dispid 1610678283;
    procedure GhostMethod_Fields_48_13; dispid 1610678284;
    procedure Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                     Attrib: FieldAttributeEnum; FieldValue: OleVariant); dispid 3;
    procedure Update; dispid 5;
    procedure Resync(ResyncValues: ResyncEnum); dispid 6;
    procedure CancelUpdate; dispid 7;
    procedure GhostMethod_Fields20_0_1; dispid 1610678272;
    procedure GhostMethod_Fields20_4_2; dispid 1610678273;
    procedure GhostMethod_Fields20_8_3; dispid 1610678274;
    procedure GhostMethod_Fields20_12_4; dispid 1610678275;
    procedure GhostMethod_Fields20_16_5; dispid 1610678276;
    procedure GhostMethod_Fields20_20_6; dispid 1610678277;
    procedure GhostMethod_Fields20_24_7; dispid 1610678278;
    procedure GhostMethod_Fields20_28_8; dispid 1610678279;
    procedure GhostMethod_Fields20_32_9; dispid 1610678280;
    procedure GhostMethod_Fields20_36_10; dispid 1610678281;
    procedure GhostMethod_Fields20_40_11; dispid 1610678282;
    procedure _Append(const Name: WideString; Type_: DataTypeEnum; DefinedSize: ADO_LONGPTR; 
                      Attrib: FieldAttributeEnum); dispid 1610874880;
    procedure Delete(Index: OleVariant); dispid 4;
    property Item[Index: OleVariant]: Field readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Field20
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field20 = interface(_ADO)
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: ADO_LONGPTR; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: ADO_LONGPTR; safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    function Get_DataFormat: IUnknown; safecall;
    procedure _Set_DataFormat(const ppiDF: IUnknown); safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    procedure Set_NumericScale(pbNumericScale: Byte); safecall;
    procedure Set_type_(pDataType: DataTypeEnum); safecall;
    procedure Set_DefinedSize(pl: ADO_LONGPTR); safecall;
    procedure Set_Attributes(pl: Integer); safecall;
    property ActualSize: ADO_LONGPTR read Get_ActualSize;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property DefinedSize: ADO_LONGPTR read Get_DefinedSize write Set_DefinedSize;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_ write Set_type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
    property DataFormat: IUnknown read Get_DataFormat write _Set_DataFormat;
  end;

// *********************************************************************//
// DispIntf:  Field20Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field20Disp = dispinterface
    ['{0000054C-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: ADO_LONGPTR dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Field
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field = interface(Field20)
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Field_0_1; safecall;
    procedure GhostMethod_Field_4_2; safecall;
    procedure GhostMethod_Field_8_3; safecall;
    procedure GhostMethod_Field_12_4; safecall;
    procedure GhostMethod_Field_16_5; safecall;
    procedure GhostMethod_Field_20_6; safecall;
    procedure GhostMethod_Field_24_7; safecall;
    procedure GhostMethod_Field_28_8; safecall;
    procedure GhostMethod_Field_32_9; safecall;
    procedure GhostMethod_Field_36_10; safecall;
    procedure GhostMethod_Field_40_11; safecall;
    procedure GhostMethod_Field_44_12; safecall;
    procedure GhostMethod_Field_48_13; safecall;
    procedure GhostMethod_Field_52_14; safecall;
    procedure GhostMethod_Field_56_15; safecall;
    procedure GhostMethod_Field_60_16; safecall;
    procedure GhostMethod_Field_64_17; safecall;
    procedure GhostMethod_Field_68_18; safecall;
    procedure GhostMethod_Field_72_19; safecall;
    procedure GhostMethod_Field_76_20; safecall;
    procedure GhostMethod_Field_80_21; safecall;
    procedure GhostMethod_Field_84_22; safecall;
    procedure GhostMethod_Field_88_23; safecall;
    procedure GhostMethod_Field_92_24; safecall;
    procedure GhostMethod_Field_96_25; safecall;
    procedure GhostMethod_Field_100_26; safecall;
    procedure GhostMethod_Field_104_27; safecall;
    procedure GhostMethod_Field_108_28; safecall;
    function Get_Status: Integer; safecall;
    property Status: Integer read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  FieldDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000569-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  FieldDisp = dispinterface
    ['{00000569-0000-0010-8000-00AA006D2EA4}']
    procedure GhostMethod_Field_0_1; dispid 1610678272;
    procedure GhostMethod_Field_4_2; dispid 1610678273;
    procedure GhostMethod_Field_8_3; dispid 1610678274;
    procedure GhostMethod_Field_12_4; dispid 1610678275;
    procedure GhostMethod_Field_16_5; dispid 1610678276;
    procedure GhostMethod_Field_20_6; dispid 1610678277;
    procedure GhostMethod_Field_24_7; dispid 1610678278;
    procedure GhostMethod_Field_28_8; dispid 1610678279;
    procedure GhostMethod_Field_32_9; dispid 1610678280;
    procedure GhostMethod_Field_36_10; dispid 1610678281;
    procedure GhostMethod_Field_40_11; dispid 1610678282;
    procedure GhostMethod_Field_44_12; dispid 1610678283;
    procedure GhostMethod_Field_48_13; dispid 1610678284;
    procedure GhostMethod_Field_52_14; dispid 1610678285;
    procedure GhostMethod_Field_56_15; dispid 1610678286;
    procedure GhostMethod_Field_60_16; dispid 1610678287;
    procedure GhostMethod_Field_64_17; dispid 1610678288;
    procedure GhostMethod_Field_68_18; dispid 1610678289;
    procedure GhostMethod_Field_72_19; dispid 1610678290;
    procedure GhostMethod_Field_76_20; dispid 1610678291;
    procedure GhostMethod_Field_80_21; dispid 1610678292;
    procedure GhostMethod_Field_84_22; dispid 1610678293;
    procedure GhostMethod_Field_88_23; dispid 1610678294;
    procedure GhostMethod_Field_92_24; dispid 1610678295;
    procedure GhostMethod_Field_96_25; dispid 1610678296;
    procedure GhostMethod_Field_100_26; dispid 1610678297;
    procedure GhostMethod_Field_104_27; dispid 1610678298;
    procedure GhostMethod_Field_108_28; dispid 1610678299;
    property Status: Integer readonly dispid 1116;
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer dispid 1114;
    property DefinedSize: ADO_LONGPTR dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte dispid 1112;
    property NumericScale: Byte dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property DataFormat: IUnknown dispid 1115;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Parameter
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Parameter = interface(_ADO)
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pbstr: WideString); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_type_: DataTypeEnum; safecall;
    procedure Set_type_(psDataType: DataTypeEnum); safecall;
    procedure Set_Direction(plParmDirection: ParameterDirectionEnum); safecall;
    function Get_Direction: ParameterDirectionEnum; safecall;
    procedure Set_Precision(pbPrecision: Byte); safecall;
    function Get_Precision: Byte; safecall;
    procedure Set_NumericScale(pbScale: Byte); safecall;
    function Get_NumericScale: Byte; safecall;
    procedure Set_Size(pl: ADO_LONGPTR); safecall;
    function Get_Size: ADO_LONGPTR; safecall;
    procedure AppendChunk(Val: OleVariant); safecall;
    function Get_Attributes: Integer; safecall;
    procedure Set_Attributes(plParmAttribs: Integer); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: OleVariant read Get_Value write Set_Value;
    property type_: DataTypeEnum read Get_type_ write Set_type_;
    property Direction: ParameterDirectionEnum read Get_Direction write Set_Direction;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property Size: ADO_LONGPTR read Get_Size write Set_Size;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  _ParameterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _ParameterDisp = dispinterface
    ['{0000050C-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 1;
    property Value: OleVariant dispid 0;
    property type_: DataTypeEnum dispid 2;
    property Direction: ParameterDirectionEnum dispid 3;
    property Precision: Byte dispid 4;
    property NumericScale: Byte dispid 5;
    property Size: ADO_LONGPTR dispid 6;
    procedure AppendChunk(Val: OleVariant); dispid 7;
    property Attributes: Integer dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: Parameters
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Parameters = interface(_DynaCollection)
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Index: OleVariant): _Parameter; safecall;
    property Item[Index: OleVariant]: _Parameter read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  ParametersDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000050D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ParametersDisp = dispinterface
    ['{0000050D-0000-0010-8000-00AA006D2EA4}']
    property Item[Index: OleVariant]: _Parameter readonly dispid 0; default;
    procedure Append(const Object_: IDispatch); dispid 1610809344;
    procedure Delete(Index: OleVariant); dispid 1610809345;
    property Count: Integer readonly dispid 1;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 2;
  end;

// *********************************************************************//
// Interface: Command25
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command25 = interface(Command15)
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    function Get_State: Integer; safecall;
    procedure Cancel; safecall;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  Command25Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000054E-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Command25Disp = dispinterface
    ['{0000054E-0000-0010-8000-00AA006D2EA4}']
    property State: Integer readonly dispid 9;
    procedure Cancel; dispid 10;
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Command
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B08400BD-F9D1-4D02-B856-71D5DBA123E9}
// *********************************************************************//
  _Command = interface(Command25)
    ['{B08400BD-F9D1-4D02-B856-71D5DBA123E9}']
    procedure _Set_CommandStream(const pvStream: IUnknown); safecall;
    function Get_CommandStream: OleVariant; safecall;
    procedure Set_Dialect(const pbstrDialect: WideString); safecall;
    function Get_Dialect: WideString; safecall;
    procedure Set_NamedParameters(pfNamedParameters: WordBool); safecall;
    function Get_NamedParameters: WordBool; safecall;
    property Dialect: WideString read Get_Dialect write Set_Dialect;
    property NamedParameters: WordBool read Get_NamedParameters write Set_NamedParameters;
  end;

// *********************************************************************//
// DispIntf:  _CommandDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B08400BD-F9D1-4D02-B856-71D5DBA123E9}
// *********************************************************************//
  _CommandDisp = dispinterface
    ['{B08400BD-F9D1-4D02-B856-71D5DBA123E9}']
    function CommandStream: IUnknown; dispid 11;
    property Dialect: WideString dispid 12;
    property NamedParameters: WordBool dispid 13;
    property State: Integer readonly dispid 9;
    procedure Cancel; dispid 10;
    function ActiveConnection: _Connection; dispid 1;
    property CommandText: WideString dispid 2;
    property CommandTimeout: Integer dispid 3;
    property Prepared: WordBool dispid 4;
    function Execute(out RecordsAffected: OleVariant; var Parameters: OleVariant; Options: Integer): _Recordset; dispid 5;
    function CreateParameter(const Name: WideString; Type_: DataTypeEnum; 
                             Direction: ParameterDirectionEnum; Size: ADO_LONGPTR; Value: OleVariant): _Parameter; dispid 6;
    property Parameters: Parameters readonly dispid 0;
    property CommandType: CommandTypeEnum dispid 7;
    property Name: WideString dispid 8;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: ConnectionEventsVt
// Flags:     (16) Hidden
// GUID:      {00000402-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ConnectionEventsVt = interface(IUnknown)
    ['{00000402-0000-0010-8000-00AA006D2EA4}']
    function InfoMessage(const pError: Error; var adStatus: EventStatusEnum; 
                         const pConnection: _Connection): HResult; stdcall;
    function BeginTransComplete(TransactionLevel: Integer; const pError: Error; 
                                var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
    function CommitTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                 const pConnection: _Connection): HResult; stdcall;
    function RollbackTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                   const pConnection: _Connection): HResult; stdcall;
    function WillExecute(var Source: WideString; var CursorType: CursorTypeEnum; 
                         var LockType: LockTypeEnum; var Options: Integer; 
                         var adStatus: EventStatusEnum; const pCommand: _Command; 
                         const pRecordset: _Recordset; const pConnection: _Connection): HResult; stdcall;
    function ExecuteComplete(RecordsAffected: Integer; const pError: Error; 
                             var adStatus: EventStatusEnum; const pCommand: _Command; 
                             const pRecordset: _Recordset; const pConnection: _Connection): HResult; stdcall;
    function WillConnect(var ConnectionString: WideString; var UserID: WideString; 
                         var Password: WideString; var Options: Integer; 
                         var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
    function ConnectComplete(const pError: Error; var adStatus: EventStatusEnum; 
                             const pConnection: _Connection): HResult; stdcall;
    function Disconnect(var adStatus: EventStatusEnum; const pConnection: _Connection): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: RecordsetEventsVt
// Flags:     (16) Hidden
// GUID:      {00000403-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  RecordsetEventsVt = interface(IUnknown)
    ['{00000403-0000-0010-8000-00AA006D2EA4}']
    function WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: EventStatusEnum; 
                             const pRecordset: _Recordset): HResult; stdcall;
    function FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: Error; 
                                 var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer; 
                              var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer; 
                                  const pError: Error; var adStatus: EventStatusEnum; 
                                  const pRecordset: _Recordset): HResult; stdcall;
    function WillChangeRecordset(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                                 const pRecordset: _Recordset): HResult; stdcall;
    function RecordsetChangeComplete(adReason: EventReasonEnum; const pError: Error; 
                                     var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function WillMove(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                      const pRecordset: _Recordset): HResult; stdcall;
    function MoveComplete(adReason: EventReasonEnum; const pError: Error; 
                          var adStatus: EventStatusEnum; const pRecordset: _Recordset): HResult; stdcall;
    function EndOfRecordset(var fMoreData: WordBool; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset): HResult; stdcall;
    function FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: EventStatusEnum; 
                           const pRecordset: _Recordset): HResult; stdcall;
    function FetchComplete(const pError: Error; var adStatus: EventStatusEnum; 
                           const pRecordset: _Recordset): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  ConnectionEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000400-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ConnectionEvents = dispinterface
    ['{00000400-0000-0010-8000-00AA006D2EA4}']
    procedure InfoMessage(const pError: Error; var adStatus: EventStatusEnum; 
                          const pConnection: _Connection); dispid 0;
    procedure BeginTransComplete(TransactionLevel: Integer; const pError: Error; 
                                 var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 1;
    procedure CommitTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                  const pConnection: _Connection); dispid 3;
    procedure RollbackTransComplete(const pError: Error; var adStatus: EventStatusEnum; 
                                    const pConnection: _Connection); dispid 2;
    procedure WillExecute(var Source: WideString; var CursorType: CursorTypeEnum; 
                          var LockType: LockTypeEnum; var Options: Integer; 
                          var adStatus: EventStatusEnum; const pCommand: _Command; 
                          const pRecordset: _Recordset; const pConnection: _Connection); dispid 4;
    procedure ExecuteComplete(RecordsAffected: Integer; const pError: Error; 
                              var adStatus: EventStatusEnum; const pCommand: _Command; 
                              const pRecordset: _Recordset; const pConnection: _Connection); dispid 5;
    procedure WillConnect(var ConnectionString: WideString; var UserID: WideString; 
                          var Password: WideString; var Options: Integer; 
                          var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 6;
    procedure ConnectComplete(const pError: Error; var adStatus: EventStatusEnum; 
                              const pConnection: _Connection); dispid 7;
    procedure Disconnect(var adStatus: EventStatusEnum; const pConnection: _Connection); dispid 8;
  end;

// *********************************************************************//
// DispIntf:  RecordsetEvents
// Flags:     (4096) Dispatchable
// GUID:      {00000266-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  RecordsetEvents = dispinterface
    ['{00000266-0000-0010-8000-00AA006D2EA4}']
    procedure WillChangeField(cFields: Integer; Fields: OleVariant; var adStatus: EventStatusEnum; 
                              const pRecordset: _Recordset); dispid 9;
    procedure FieldChangeComplete(cFields: Integer; Fields: OleVariant; const pError: Error; 
                                  var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 10;
    procedure WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer; 
                               var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 11;
    procedure RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer; 
                                   const pError: Error; var adStatus: EventStatusEnum; 
                                   const pRecordset: _Recordset); dispid 12;
    procedure WillChangeRecordset(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                                  const pRecordset: _Recordset); dispid 13;
    procedure RecordsetChangeComplete(adReason: EventReasonEnum; const pError: Error; 
                                      var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 14;
    procedure WillMove(adReason: EventReasonEnum; var adStatus: EventStatusEnum; 
                       const pRecordset: _Recordset); dispid 15;
    procedure MoveComplete(adReason: EventReasonEnum; const pError: Error; 
                           var adStatus: EventStatusEnum; const pRecordset: _Recordset); dispid 16;
    procedure EndOfRecordset(var fMoreData: WordBool; var adStatus: EventStatusEnum; 
                             const pRecordset: _Recordset); dispid 17;
    procedure FetchProgress(Progress: Integer; MaxProgress: Integer; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset); dispid 18;
    procedure FetchComplete(const pError: Error; var adStatus: EventStatusEnum; 
                            const pRecordset: _Recordset); dispid 19;
  end;

// *********************************************************************//
// Interface: ADOConnectionConstruction15
// Flags:     (512) Restricted
// GUID:      {00000516-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOConnectionConstruction15 = interface(IUnknown)
    ['{00000516-0000-0010-8000-00AA006D2EA4}']
    function Get_DSO(out ppDSO: IUnknown): HResult; stdcall;
    function Get_Session(out ppSession: IUnknown): HResult; stdcall;
    function WrapDSOandSession(const pDSO: IUnknown; const pSession: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOConnectionConstruction
// Flags:     (512) Restricted
// GUID:      {00000551-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOConnectionConstruction = interface(ADOConnectionConstruction15)
    ['{00000551-0000-0010-8000-00AA006D2EA4}']
  end;

// *********************************************************************//
// Interface: _Record
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Record = interface(_ADO)
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function Get_ActiveConnection: OleVariant; safecall;
    procedure Set_ActiveConnection(const pvar: WideString); safecall;
    procedure _Set_ActiveConnection(const pvar: _Connection); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Source: OleVariant; safecall;
    procedure Set_Source(const pvar: WideString); safecall;
    procedure _Set_Source(const pvar: IDispatch); safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_ParentURL: WideString; safecall;
    function MoveRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; safecall;
    function CopyRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; safecall;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); safecall;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    function Get_Fields: Fields; safecall;
    function Get_RecordType: RecordTypeEnum; safecall;
    function GetChildren: _Recordset; safecall;
    procedure Cancel; safecall;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property ParentURL: WideString read Get_ParentURL;
    property Fields: Fields read Get_Fields;
    property RecordType: RecordTypeEnum read Get_RecordType;
  end;

// *********************************************************************//
// DispIntf:  _RecordDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000562-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _RecordDisp = dispinterface
    ['{00000562-0000-0010-8000-00AA006D2EA4}']
    function ActiveConnection: OleVariant; dispid 1;
    property State: ObjectStateEnum readonly dispid 2;
    function Source: OleVariant; dispid 3;
    property Mode: ConnectModeEnum dispid 4;
    property ParentURL: WideString readonly dispid 5;
    function MoveRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString; dispid 6;
    function CopyRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString; dispid 7;
    procedure DeleteRecord(const Source: WideString; Async: WordBool); dispid 8;
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); dispid 9;
    procedure Close; dispid 10;
    property Fields: Fields readonly dispid 0;
    property RecordType: RecordTypeEnum readonly dispid 11;
    function GetChildren: _Recordset; dispid 12;
    procedure Cancel; dispid 13;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// Interface: _Stream
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _Stream = interface(IDispatch)
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    function Get_Size: ADO_LONGPTR; safecall;
    function Get_EOS: WordBool; safecall;
    function Get_Position: ADO_LONGPTR; safecall;
    procedure Set_Position(pPos: ADO_LONGPTR); safecall;
    function Get_type_: StreamTypeEnum; safecall;
    procedure Set_type_(ptype: StreamTypeEnum); safecall;
    function Get_LineSeparator: LineSeparatorEnum; safecall;
    procedure Set_LineSeparator(pLS: LineSeparatorEnum); safecall;
    function Get_State: ObjectStateEnum; safecall;
    function Get_Mode: ConnectModeEnum; safecall;
    procedure Set_Mode(pMode: ConnectModeEnum); safecall;
    function Get_Charset: WideString; safecall;
    procedure Set_Charset(const pbstrCharset: WideString); safecall;
    function Read(NumBytes: Integer): OleVariant; safecall;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); safecall;
    procedure Close; safecall;
    procedure SkipLine; safecall;
    procedure Write(Buffer: OleVariant); safecall;
    procedure SetEOS; safecall;
    procedure CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR); safecall;
    procedure Flush; safecall;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    function ReadText(NumChars: Integer): WideString; safecall;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); safecall;
    procedure Cancel; safecall;
    property Size: ADO_LONGPTR read Get_Size;
    property EOS: WordBool read Get_EOS;
    property Position: ADO_LONGPTR read Get_Position write Set_Position;
    property type_: StreamTypeEnum read Get_type_ write Set_type_;
    property LineSeparator: LineSeparatorEnum read Get_LineSeparator write Set_LineSeparator;
    property State: ObjectStateEnum read Get_State;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Charset: WideString read Get_Charset write Set_Charset;
  end;

// *********************************************************************//
// DispIntf:  _StreamDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {00000565-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  _StreamDisp = dispinterface
    ['{00000565-0000-0010-8000-00AA006D2EA4}']
    property Size: ADO_LONGPTR readonly dispid 1;
    property EOS: WordBool readonly dispid 2;
    property Position: ADO_LONGPTR dispid 3;
    property type_: StreamTypeEnum dispid 4;
    property LineSeparator: LineSeparatorEnum dispid 5;
    property State: ObjectStateEnum readonly dispid 6;
    property Mode: ConnectModeEnum dispid 7;
    property Charset: WideString dispid 8;
    function Read(NumBytes: Integer): OleVariant; dispid 9;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString); dispid 10;
    procedure Close; dispid 11;
    procedure SkipLine; dispid 12;
    procedure Write(Buffer: OleVariant); dispid 13;
    procedure SetEOS; dispid 14;
    procedure CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR); dispid 15;
    procedure Flush; dispid 16;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum); dispid 17;
    procedure LoadFromFile(const FileName: WideString); dispid 18;
    function ReadText(NumChars: Integer): WideString; dispid 19;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum); dispid 20;
    procedure Cancel; dispid 21;
  end;

// *********************************************************************//
// Interface: ADORecordConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000567-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADORecordConstruction = interface(IDispatch)
    ['{00000567-0000-0010-8000-00AA006D2EA4}']
    function Get_Row(out ppRow: IUnknown): HResult; stdcall;
    function Set_Row(const ppRow: IUnknown): HResult; stdcall;
    function Set_ParentRow(const Param1: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOStreamConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000568-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOStreamConstruction = interface(IDispatch)
    ['{00000568-0000-0010-8000-00AA006D2EA4}']
    function Get_Stream(out ppStm: IUnknown): HResult; stdcall;
    function Set_Stream(const ppStm: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADOCommandConstruction
// Flags:     (512) Restricted
// GUID:      {00000517-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADOCommandConstruction = interface(IUnknown)
    ['{00000517-0000-0010-8000-00AA006D2EA4}']
    function Get_OLEDBCommand(out ppOLEDBCommand: IUnknown): HResult; stdcall;
    function Set_OLEDBCommand(const ppOLEDBCommand: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ADORecordsetConstruction
// Flags:     (4608) Restricted Dispatchable
// GUID:      {00000283-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  ADORecordsetConstruction = interface(IDispatch)
    ['{00000283-0000-0010-8000-00AA006D2EA4}']
    function Get_Rowset(out ppRowset: IUnknown): HResult; stdcall;
    function Set_Rowset(const ppRowset: IUnknown): HResult; stdcall;
    function Get_Chapter(out plChapter: ADO_LONGPTR): HResult; stdcall;
    function Set_Chapter(plChapter: ADO_LONGPTR): HResult; stdcall;
    function Get_RowPosition(out ppRowPos: IUnknown): HResult; stdcall;
    function Set_RowPosition(const ppRowPos: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: Field15
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field15 = interface(_ADO)
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    function Get_ActualSize: ADO_LONGPTR; safecall;
    function Get_Attributes: Integer; safecall;
    function Get_DefinedSize: ADO_LONGPTR; safecall;
    function Get_Name: WideString; safecall;
    function Get_type_: DataTypeEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pvar: OleVariant); safecall;
    function Get_Precision: Byte; safecall;
    function Get_NumericScale: Byte; safecall;
    procedure AppendChunk(Data: OleVariant); safecall;
    function GetChunk(Length: Integer): OleVariant; safecall;
    function Get_OriginalValue: OleVariant; safecall;
    function Get_UnderlyingValue: OleVariant; safecall;
    property ActualSize: ADO_LONGPTR read Get_ActualSize;
    property Attributes: Integer read Get_Attributes;
    property DefinedSize: ADO_LONGPTR read Get_DefinedSize;
    property Name: WideString read Get_Name;
    property type_: DataTypeEnum read Get_type_;
    property Value: OleVariant read Get_Value write Set_Value;
    property Precision: Byte read Get_Precision;
    property NumericScale: Byte read Get_NumericScale;
    property OriginalValue: OleVariant read Get_OriginalValue;
    property UnderlyingValue: OleVariant read Get_UnderlyingValue;
  end;

// *********************************************************************//
// DispIntf:  Field15Disp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000505-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  Field15Disp = dispinterface
    ['{00000505-0000-0010-8000-00AA006D2EA4}']
    property ActualSize: ADO_LONGPTR readonly dispid 1109;
    property Attributes: Integer readonly dispid 1114;
    property DefinedSize: ADO_LONGPTR readonly dispid 1103;
    property Name: WideString readonly dispid 1100;
    property type_: DataTypeEnum readonly dispid 1102;
    property Value: OleVariant dispid 0;
    property Precision: Byte readonly dispid 1112;
    property NumericScale: Byte readonly dispid 1113;
    procedure AppendChunk(Data: OleVariant); dispid 1107;
    function GetChunk(Length: Integer): OleVariant; dispid 1108;
    property OriginalValue: OleVariant readonly dispid 1104;
    property UnderlyingValue: OleVariant readonly dispid 1105;
    property Properties: Properties readonly dispid 500;
  end;

// *********************************************************************//
// The Class CoConnection provides a Create and CreateRemote method to          
// create instances of the default interface _Connection exposed by              
// the CoClass Connection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoConnection = class
    class function Create: _Connection;
    class function CreateRemote(const MachineName: string): _Connection;
  end;

  TCoConnectionInfoMessage = procedure(ASender: TObject; const pError: Error; 
                                                         var adStatus: EventStatusEnum; 
                                                         const pConnection: _Connection) of object;
  TCoConnectionBeginTransComplete = procedure(ASender: TObject; TransactionLevel: Integer; 
                                                                const pError: Error; 
                                                                var adStatus: EventStatusEnum; 
                                                                const pConnection: _Connection) of object;
  TCoConnectionCommitTransComplete = procedure(ASender: TObject; const pError: Error; 
                                                                 var adStatus: EventStatusEnum; 
                                                                 const pConnection: _Connection) of object;
  TCoConnectionRollbackTransComplete = procedure(ASender: TObject; const pError: Error; 
                                                                   var adStatus: EventStatusEnum; 
                                                                   const pConnection: _Connection) of object;
  TCoConnectionWillExecute = procedure(ASender: TObject; var Source: WideString; 
                                                         var CursorType: CursorTypeEnum; 
                                                         var LockType: LockTypeEnum; 
                                                         var Options: Integer; 
                                                         var adStatus: EventStatusEnum; 
                                                         const pCommand: _Command; 
                                                         const pRecordset: _Recordset; 
                                                         const pConnection: _Connection) of object;
  TCoConnectionExecuteComplete = procedure(ASender: TObject; RecordsAffected: Integer; 
                                                             const pError: Error; 
                                                             var adStatus: EventStatusEnum; 
                                                             const pCommand: _Command; 
                                                             const pRecordset: _Recordset; 
                                                             const pConnection: _Connection) of object;
  TCoConnectionWillConnect = procedure(ASender: TObject; var ConnectionString: WideString; 
                                                         var UserID: WideString; 
                                                         var Password: WideString; 
                                                         var Options: Integer; 
                                                         var adStatus: EventStatusEnum; 
                                                         const pConnection: _Connection) of object;
  TCoConnectionConnectComplete = procedure(ASender: TObject; const pError: Error; 
                                                             var adStatus: EventStatusEnum; 
                                                             const pConnection: _Connection) of object;
  TCoConnectionDisconnect = procedure(ASender: TObject; var adStatus: EventStatusEnum; 
                                                        const pConnection: _Connection) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoConnection
// Help String      : 
// Default Interface: _Connection
// Def. Intf. DISP? : No
// Event   Interface: ConnectionEvents
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoConnection = class(TOleServer)
  private
    FOnInfoMessage: TCoConnectionInfoMessage;
    FOnBeginTransComplete: TCoConnectionBeginTransComplete;
    FOnCommitTransComplete: TCoConnectionCommitTransComplete;
    FOnRollbackTransComplete: TCoConnectionRollbackTransComplete;
    FOnWillExecute: TCoConnectionWillExecute;
    FOnExecuteComplete: TCoConnectionExecuteComplete;
    FOnWillConnect: TCoConnectionWillConnect;
    FOnConnectComplete: TCoConnectionConnectComplete;
    FOnDisconnect: TCoConnectionDisconnect;
    FIntf:        _Connection;
    function      GetDefaultInterface: _Connection;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_ConnectionString: WideString;
    procedure Set_ConnectionString(const pbstr: WideString);
    function Get_CommandTimeout: Integer;
    procedure Set_CommandTimeout(plTimeout: Integer);
    function Get_ConnectionTimeout: Integer;
    procedure Set_ConnectionTimeout(plTimeout: Integer);
    function Get_Version: WideString;
    function Get_Errors: Errors;
    function Get_DefaultDatabase: WideString;
    procedure Set_DefaultDatabase(const pbstr: WideString);
    function Get_IsolationLevel: IsolationLevelEnum;
    procedure Set_IsolationLevel(Level: IsolationLevelEnum);
    function Get_Attributes: Integer;
    procedure Set_Attributes(plAttr: Integer);
    function Get_CursorLocation: CursorLocationEnum;
    procedure Set_CursorLocation(plCursorLoc: CursorLocationEnum);
    function Get_Mode: ConnectModeEnum;
    procedure Set_Mode(plMode: ConnectModeEnum);
    function Get_Provider: WideString;
    procedure Set_Provider(const pbstr: WideString);
    function Get_State: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Connection);
    procedure Disconnect; override;
    procedure Close;
    function Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                     Options: Integer): _Recordset;
    function BeginTrans: Integer;
    procedure CommitTrans;
    procedure RollbackTrans;
    procedure Open(const ConnectionString: WideString; const UserID: WideString; 
                   const Password: WideString; Options: Integer);
    function OpenSchema(Schema: SchemaEnum): _Recordset; overload;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant): _Recordset; overload;
    function OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset; overload;
    procedure Cancel;
    property DefaultInterface: _Connection read GetDefaultInterface;
    property Version: WideString read Get_Version;
    property Errors: Errors read Get_Errors;
    property State: Integer read Get_State;
    property ConnectionString: WideString read Get_ConnectionString write Set_ConnectionString;
    property CommandTimeout: Integer read Get_CommandTimeout write Set_CommandTimeout;
    property ConnectionTimeout: Integer read Get_ConnectionTimeout write Set_ConnectionTimeout;
    property DefaultDatabase: WideString read Get_DefaultDatabase write Set_DefaultDatabase;
    property IsolationLevel: IsolationLevelEnum read Get_IsolationLevel write Set_IsolationLevel;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
    property CursorLocation: CursorLocationEnum read Get_CursorLocation write Set_CursorLocation;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Provider: WideString read Get_Provider write Set_Provider;
  published
    property OnInfoMessage: TCoConnectionInfoMessage read FOnInfoMessage write FOnInfoMessage;
    property OnBeginTransComplete: TCoConnectionBeginTransComplete read FOnBeginTransComplete write FOnBeginTransComplete;
    property OnCommitTransComplete: TCoConnectionCommitTransComplete read FOnCommitTransComplete write FOnCommitTransComplete;
    property OnRollbackTransComplete: TCoConnectionRollbackTransComplete read FOnRollbackTransComplete write FOnRollbackTransComplete;
    property OnWillExecute: TCoConnectionWillExecute read FOnWillExecute write FOnWillExecute;
    property OnExecuteComplete: TCoConnectionExecuteComplete read FOnExecuteComplete write FOnExecuteComplete;
    property OnWillConnect: TCoConnectionWillConnect read FOnWillConnect write FOnWillConnect;
    property OnConnectComplete: TCoConnectionConnectComplete read FOnConnectComplete write FOnConnectComplete;
    property OnDisconnect: TCoConnectionDisconnect read FOnDisconnect write FOnDisconnect;
  end;



// *********************************************************************//
// The Class CoRecord_ provides a Create and CreateRemote method to
// create instances of the default interface _Record exposed by
// the CoClass Record_. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoRecord_ = class
    class function Create: _Record;
    class function CreateRemote(const MachineName: string): _Record;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoRecord
// Help String      :
// Default Interface: _Record
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoRecord = class(TOleServer)
  private
    FIntf:        _Record;
    function      GetDefaultInterface: _Record;
  protected
    procedure InitServerData; override;
    function Get_ActiveConnection: OleVariant;
    procedure Set_ActiveConnection(const pvar: WideString);
    procedure _Set_ActiveConnection(const pvar: _Connection);
    function Get_State: ObjectStateEnum;
    function Get_Source: OleVariant;
    procedure Set_Source(const pvar: WideString);
    procedure _Set_Source(const pvar: IDispatch);
    function Get_Mode: ConnectModeEnum;
    procedure Set_Mode(pMode: ConnectModeEnum);
    function Get_ParentURL: WideString;
    function Get_Fields: Fields;
    function Get_RecordType: RecordTypeEnum;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Record);
    procedure Disconnect; override;
    function MoveRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: MoveRecordOptionsEnum; Async: WordBool): WideString;
    function CopyRecord(const Source: WideString; const Destination: WideString; 
                        const UserName: WideString; const Password: WideString; 
                        Options: CopyRecordOptionsEnum; Async: WordBool): WideString;
    procedure DeleteRecord(const Source: WideString; Async: WordBool);
    procedure Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                   CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString);
    procedure Close;
    function GetChildren: _Recordset; reintroduce;
    procedure Cancel;
    property DefaultInterface: _Record read GetDefaultInterface;
    property State: ObjectStateEnum read Get_State;
    property ParentURL: WideString read Get_ParentURL;
    property Fields: Fields read Get_Fields;
    property RecordType: RecordTypeEnum read Get_RecordType;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
  end;



// *********************************************************************//
// The Class CoStream provides a Create and CreateRemote method to
// create instances of the default interface _Stream exposed by
// the CoClass Stream. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoStream = class
    class function Create: _Stream;
    class function CreateRemote(const MachineName: string): _Stream;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoStream
// Help String      :
// Default Interface: _Stream
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoStream = class(TOleServer)
  private
    FIntf:        _Stream;
    function      GetDefaultInterface: _Stream;
  protected
    procedure InitServerData; override;
    function Get_Size: ADO_LONGPTR;
    function Get_EOS: WordBool;
    function Get_Position: ADO_LONGPTR;
    procedure Set_Position(pPos: ADO_LONGPTR);
    function Get_type_: StreamTypeEnum;
    procedure Set_type_(ptype: StreamTypeEnum);
    function Get_LineSeparator: LineSeparatorEnum;
    procedure Set_LineSeparator(pLS: LineSeparatorEnum);
    function Get_State: ObjectStateEnum;
    function Get_Mode: ConnectModeEnum;
    procedure Set_Mode(pMode: ConnectModeEnum);
    function Get_Charset: WideString;
    procedure Set_Charset(const pbstrCharset: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Stream);
    procedure Disconnect; override;
    function Read(NumBytes: Integer): OleVariant;
    procedure Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                   const UserName: WideString; const Password: WideString);
    procedure Close;
    procedure SkipLine;
    procedure Write(Buffer: OleVariant);
    procedure SetEOS;
    procedure CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR);
    procedure Flush;
    procedure SaveToFile(const FileName: WideString; Options: SaveOptionsEnum);
    procedure LoadFromFile(const FileName: WideString);
    function ReadText(NumChars: Integer): WideString;
    procedure WriteText(const Data: WideString; Options: StreamWriteEnum);
    procedure Cancel;
    property DefaultInterface: _Stream read GetDefaultInterface;
    property Size: ADO_LONGPTR read Get_Size;
    property EOS: WordBool read Get_EOS;
    property State: ObjectStateEnum read Get_State;
    property Position: ADO_LONGPTR read Get_Position write Set_Position;
    property type_: StreamTypeEnum read Get_type_ write Set_type_;
    property LineSeparator: LineSeparatorEnum read Get_LineSeparator write Set_LineSeparator;
    property Mode: ConnectModeEnum read Get_Mode write Set_Mode;
    property Charset: WideString read Get_Charset write Set_Charset;
  end;



// *********************************************************************//
// The Class CoCommand provides a Create and CreateRemote method to
// create instances of the default interface _Command exposed by
// the CoClass Command. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoCommand = class
    class function Create: _Command;
    class function CreateRemote(const MachineName: string): _Command;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoCommand
// Help String      :
// Default Interface: _Command
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoCommand = class(TOleServer)
  private
    FIntf:        _Command;
    function      GetDefaultInterface: _Command;
  protected
    procedure InitServerData; override;
    procedure _Set_CommandStream(const pvStream: IUnknown);
    function Get_CommandStream: OleVariant;
    procedure Set_Dialect(const pbstrDialect: WideString);
    function Get_Dialect: WideString;
    procedure Set_NamedParameters(pfNamedParameters: WordBool);
    function Get_NamedParameters: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Command);
    procedure Disconnect; override;
    property DefaultInterface: _Command read GetDefaultInterface;
    property Dialect: WideString read Get_Dialect write Set_Dialect;
    property NamedParameters: WordBool read Get_NamedParameters write Set_NamedParameters;
  end;



// *********************************************************************//
// The Class CoRecordset provides a Create and CreateRemote method to
// create instances of the default interface _Recordset exposed by
// the CoClass Recordset. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoRecordset = class
    class function Create: _Recordset;
    class function CreateRemote(const MachineName: string): _Recordset;
  end;

  TCoRecordsetWillChangeField = procedure(ASender: TObject; cFields: Integer; Fields: OleVariant;
                                                            var adStatus: EventStatusEnum;
                                                            const pRecordset: _Recordset) of object;
  TCoRecordsetFieldChangeComplete = procedure(ASender: TObject; cFields: Integer;
                                                                Fields: OleVariant;
                                                                const pError: Error;
                                                                var adStatus: EventStatusEnum;
                                                                const pRecordset: _Recordset) of object;
  TCoRecordsetWillChangeRecord = procedure(ASender: TObject; adReason: EventReasonEnum;
                                                             cRecords: Integer; 
                                                             var adStatus: EventStatusEnum; 
                                                             const pRecordset: _Recordset) of object;
  TCoRecordsetRecordChangeComplete = procedure(ASender: TObject; adReason: EventReasonEnum; 
                                                                 cRecords: Integer; 
                                                                 const pError: Error; 
                                                                 var adStatus: EventStatusEnum; 
                                                                 const pRecordset: _Recordset) of object;
  TCoRecordsetWillChangeRecordset = procedure(ASender: TObject; adReason: EventReasonEnum; 
                                                                var adStatus: EventStatusEnum; 
                                                                const pRecordset: _Recordset) of object;
  TCoRecordsetRecordsetChangeComplete = procedure(ASender: TObject; adReason: EventReasonEnum; 
                                                                    const pError: Error; 
                                                                    var adStatus: EventStatusEnum; 
                                                                    const pRecordset: _Recordset) of object;
  TCoRecordsetWillMove = procedure(ASender: TObject; adReason: EventReasonEnum; 
                                                     var adStatus: EventStatusEnum; 
                                                     const pRecordset: _Recordset) of object;
  TCoRecordsetMoveComplete = procedure(ASender: TObject; adReason: EventReasonEnum; 
                                                         const pError: Error; 
                                                         var adStatus: EventStatusEnum; 
                                                         const pRecordset: _Recordset) of object;
  TCoRecordsetEndOfRecordset = procedure(ASender: TObject; var fMoreData: WordBool; 
                                                           var adStatus: EventStatusEnum; 
                                                           const pRecordset: _Recordset) of object;
  TCoRecordsetFetchProgress = procedure(ASender: TObject; Progress: Integer; MaxProgress: Integer;
                                                          var adStatus: EventStatusEnum;
                                                          const pRecordset: _Recordset) of object;
  TCoRecordsetFetchComplete = procedure(ASender: TObject; const pError: Error;
                                                          var adStatus: EventStatusEnum;
                                                          const pRecordset: _Recordset) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoRecordset
// Help String      :
// Default Interface: _Recordset
// Def. Intf. DISP? : No
// Event   Interface: RecordsetEvents
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoRecordset = class(TOleServer)
  private
    FOnWillChangeField: TCoRecordsetWillChangeField;
    FOnFieldChangeComplete: TCoRecordsetFieldChangeComplete;
    FOnWillChangeRecord: TCoRecordsetWillChangeRecord;
    FOnRecordChangeComplete: TCoRecordsetRecordChangeComplete;
    FOnWillChangeRecordset: TCoRecordsetWillChangeRecordset;
    FOnRecordsetChangeComplete: TCoRecordsetRecordsetChangeComplete;
    FOnWillMove: TCoRecordsetWillMove;
    FOnMoveComplete: TCoRecordsetMoveComplete;
    FOnEndOfRecordset: TCoRecordsetEndOfRecordset;
    FOnFetchProgress: TCoRecordsetFetchProgress;
    FOnFetchComplete: TCoRecordsetFetchComplete;
    FIntf:        _Recordset;
    function      GetDefaultInterface: _Recordset;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Recordset);
    procedure Disconnect; override;
    procedure Save(Destination: OleVariant; PersistFormat: PersistFormatEnum);
    property DefaultInterface: _Recordset read GetDefaultInterface;
  published
    property OnWillChangeField: TCoRecordsetWillChangeField read FOnWillChangeField write FOnWillChangeField;
    property OnFieldChangeComplete: TCoRecordsetFieldChangeComplete read FOnFieldChangeComplete write FOnFieldChangeComplete;
    property OnWillChangeRecord: TCoRecordsetWillChangeRecord read FOnWillChangeRecord write FOnWillChangeRecord;
    property OnRecordChangeComplete: TCoRecordsetRecordChangeComplete read FOnRecordChangeComplete write FOnRecordChangeComplete;
    property OnWillChangeRecordset: TCoRecordsetWillChangeRecordset read FOnWillChangeRecordset write FOnWillChangeRecordset;
    property OnRecordsetChangeComplete: TCoRecordsetRecordsetChangeComplete read FOnRecordsetChangeComplete write FOnRecordsetChangeComplete;
    property OnWillMove: TCoRecordsetWillMove read FOnWillMove write FOnWillMove;
    property OnMoveComplete: TCoRecordsetMoveComplete read FOnMoveComplete write FOnMoveComplete;
    property OnEndOfRecordset: TCoRecordsetEndOfRecordset read FOnEndOfRecordset write FOnEndOfRecordset;
    property OnFetchProgress: TCoRecordsetFetchProgress read FOnFetchProgress write FOnFetchProgress;
    property OnFetchComplete: TCoRecordsetFetchComplete read FOnFetchComplete write FOnFetchComplete;
  end;



// *********************************************************************//
// The Class CoParameter provides a Create and CreateRemote method to
// create instances of the default interface _Parameter exposed by
// the CoClass Parameter. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoParameter = class
    class function Create: _Parameter;
    class function CreateRemote(const MachineName: string): _Parameter;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCoParameter
// Help String      :
// Default Interface: _Parameter
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (6) CanCreate Licensed
// *********************************************************************//
  TCoParameter = class(TOleServer)
  private
    FIntf:        _Parameter;
    function      GetDefaultInterface: _Parameter;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    procedure Set_Name(const pbstr: WideString);
    function Get_Value: OleVariant;
    procedure Set_Value(pvar: OleVariant);
    function Get_type_: DataTypeEnum;
    procedure Set_type_(psDataType: DataTypeEnum);
    procedure Set_Direction(plParmDirection: ParameterDirectionEnum);
    function Get_Direction: ParameterDirectionEnum;
    procedure Set_Precision(pbPrecision: Byte);
    function Get_Precision: Byte;
    procedure Set_NumericScale(pbScale: Byte);
    function Get_NumericScale: Byte;
    procedure Set_Size(pl: ADO_LONGPTR);
    function Get_Size: ADO_LONGPTR;
    function Get_Attributes: Integer;
    procedure Set_Attributes(plParmAttribs: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: _Parameter);
    procedure Disconnect; override;
    procedure AppendChunk(Val: OleVariant);
    property DefaultInterface: _Parameter read GetDefaultInterface;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: WideString read Get_Name write Set_Name;
    property type_: DataTypeEnum read Get_type_ write Set_type_;
    property Direction: ParameterDirectionEnum read Get_Direction write Set_Direction;
    property Precision: Byte read Get_Precision write Set_Precision;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property Size: ADO_LONGPTR read Get_Size write Set_Size;
    property Attributes: Integer read Get_Attributes write Set_Attributes;
  end;


implementation

uses ComObj;

class function CoConnection.Create: _Connection;
begin
  Result := CreateComObject(CLASS_Connection) as _Connection;
end;

class function CoConnection.CreateRemote(const MachineName: string): _Connection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Connection) as _Connection;
end;

procedure TCoConnection.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00000514-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{00000550-0000-0010-8000-00AA006D2EA4}';
    EventIID:  '{00000400-0000-0010-8000-00AA006D2EA4}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoConnection.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as _Connection;
  end;
end;

procedure TCoConnection.ConnectTo(svrIntf: _Connection);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCoConnection.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCoConnection.GetDefaultInterface: _Connection;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TCoConnection.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    0: if Assigned(FOnInfoMessage) then
         FOnInfoMessage(Self,
                        IUnknown(TVarData(Params[0]).VPointer) as Error {const Error},
                        EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                        IUnknown(TVarData(Params[2]).VPointer) as _Connection {const _Connection});
    1: if Assigned(FOnBeginTransComplete) then
         FOnBeginTransComplete(Self,
                               Params[0] {Integer},
                               IUnknown(TVarData(Params[1]).VPointer) as Error {const Error},
                               EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                               IUnknown(TVarData(Params[3]).VPointer) as _Connection {const _Connection});
    3: if Assigned(FOnCommitTransComplete) then
         FOnCommitTransComplete(Self,
                                IUnknown(TVarData(Params[0]).VPointer) as Error {const Error},
                                EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                                IUnknown(TVarData(Params[2]).VPointer) as _Connection {const _Connection});
    2: if Assigned(FOnRollbackTransComplete) then
         FOnRollbackTransComplete(Self,
                                  IUnknown(TVarData(Params[0]).VPointer) as Error {const Error},
                                  EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                                  IUnknown(TVarData(Params[2]).VPointer) as _Connection {const _Connection});
    4: if Assigned(FOnWillExecute) then
         FOnWillExecute(Self,
                        WideString((TVarData(Params[0]).VPointer)^) {var WideString},
                        CursorTypeEnum((TVarData(Params[1]).VPointer)^) {var CursorTypeEnum},
                        LockTypeEnum((TVarData(Params[2]).VPointer)^) {var LockTypeEnum},
                        Integer((TVarData(Params[3]).VPointer)^) {var Integer},
                        EventStatusEnum((TVarData(Params[4]).VPointer)^) {var EventStatusEnum},
                        IUnknown(TVarData(Params[5]).VPointer) as _Command {const _Command},
                        IUnknown(TVarData(Params[6]).VPointer) as _Recordset {const _Recordset},
                        IUnknown(TVarData(Params[7]).VPointer) as _Connection {const _Connection});
    5: if Assigned(FOnExecuteComplete) then
         FOnExecuteComplete(Self,
                            Params[0] {Integer},
                            IUnknown(TVarData(Params[1]).VPointer) as Error {const Error},
                            EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                            IUnknown(TVarData(Params[3]).VPointer) as _Command {const _Command},
                            IUnknown(TVarData(Params[4]).VPointer) as _Recordset {const _Recordset},
                            IUnknown(TVarData(Params[5]).VPointer) as _Connection {const _Connection});
    6: if Assigned(FOnWillConnect) then
         FOnWillConnect(Self,
                        WideString((TVarData(Params[0]).VPointer)^) {var WideString},
                        WideString((TVarData(Params[1]).VPointer)^) {var WideString},
                        WideString((TVarData(Params[2]).VPointer)^) {var WideString},
                        Integer((TVarData(Params[3]).VPointer)^) {var Integer},
                        EventStatusEnum((TVarData(Params[4]).VPointer)^) {var EventStatusEnum},
                        IUnknown(TVarData(Params[5]).VPointer) as _Connection {const _Connection});
    7: if Assigned(FOnConnectComplete) then
         FOnConnectComplete(Self,
                            IUnknown(TVarData(Params[0]).VPointer) as Error {const Error},
                            EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                            IUnknown(TVarData(Params[2]).VPointer) as _Connection {const _Connection});
    8: if Assigned(FOnDisconnect) then
         FOnDisconnect(Self,
                       EventStatusEnum((TVarData(Params[0]).VPointer)^) {var EventStatusEnum},
                       IUnknown(TVarData(Params[1]).VPointer) as _Connection {const _Connection});
  end; {case DispID}
end;

function TCoConnection.Get_ConnectionString: WideString;
begin
    Result := DefaultInterface.ConnectionString;
end;

procedure TCoConnection.Set_ConnectionString(const pbstr: WideString);
  { Warning: The property ConnectionString has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ConnectionString := pbstr;
end;

function TCoConnection.Get_CommandTimeout: Integer;
begin
    Result := DefaultInterface.CommandTimeout;
end;

procedure TCoConnection.Set_CommandTimeout(plTimeout: Integer);
begin
  DefaultInterface.Set_CommandTimeout(plTimeout);
end;

function TCoConnection.Get_ConnectionTimeout: Integer;
begin
    Result := DefaultInterface.ConnectionTimeout;
end;

procedure TCoConnection.Set_ConnectionTimeout(plTimeout: Integer);
begin
  DefaultInterface.Set_ConnectionTimeout(plTimeout);
end;

function TCoConnection.Get_Version: WideString;
begin
    Result := DefaultInterface.Version;
end;

function TCoConnection.Get_Errors: Errors;
begin
    Result := DefaultInterface.Errors;
end;

function TCoConnection.Get_DefaultDatabase: WideString;
begin
    Result := DefaultInterface.DefaultDatabase;
end;

procedure TCoConnection.Set_DefaultDatabase(const pbstr: WideString);
  { Warning: The property DefaultDatabase has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.DefaultDatabase := pbstr;
end;

function TCoConnection.Get_IsolationLevel: IsolationLevelEnum;
begin
    Result := DefaultInterface.IsolationLevel;
end;

procedure TCoConnection.Set_IsolationLevel(Level: IsolationLevelEnum);
begin
  DefaultInterface.Set_IsolationLevel(Level);
end;

function TCoConnection.Get_Attributes: Integer;
begin
    Result := DefaultInterface.Attributes;
end;

procedure TCoConnection.Set_Attributes(plAttr: Integer);
begin
  DefaultInterface.Set_Attributes(plAttr);
end;

function TCoConnection.Get_CursorLocation: CursorLocationEnum;
begin
    Result := DefaultInterface.CursorLocation;
end;

procedure TCoConnection.Set_CursorLocation(plCursorLoc: CursorLocationEnum);
begin
  DefaultInterface.Set_CursorLocation(plCursorLoc);
end;

function TCoConnection.Get_Mode: ConnectModeEnum;
begin
    Result := DefaultInterface.Mode;
end;

procedure TCoConnection.Set_Mode(plMode: ConnectModeEnum);
begin
  DefaultInterface.Set_Mode(plMode);
end;

function TCoConnection.Get_Provider: WideString;
begin
    Result := DefaultInterface.Provider;
end;

procedure TCoConnection.Set_Provider(const pbstr: WideString);
  { Warning: The property Provider has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Provider := pbstr;
end;

function TCoConnection.Get_State: Integer;
begin
    Result := DefaultInterface.State;
end;

procedure TCoConnection.Close;
begin
  DefaultInterface.Close;
end;

function TCoConnection.Execute(const CommandText: WideString; out RecordsAffected: OleVariant; 
                               Options: Integer): _Recordset;
begin
  Result := DefaultInterface.Execute(CommandText, RecordsAffected, Options);
end;

function TCoConnection.BeginTrans: Integer;
begin
  Result := DefaultInterface.BeginTrans;
end;

procedure TCoConnection.CommitTrans;
begin
  DefaultInterface.CommitTrans;
end;

procedure TCoConnection.RollbackTrans;
begin
  DefaultInterface.RollbackTrans;
end;

procedure TCoConnection.Open(const ConnectionString: WideString; const UserID: WideString; 
                             const Password: WideString; Options: Integer);
begin
  DefaultInterface.Open(ConnectionString, UserID, Password, Options);
end;

function TCoConnection.OpenSchema(Schema: SchemaEnum): _Recordset;
begin
  Result := DefaultInterface.OpenSchema(Schema, EmptyParam, EmptyParam);
end;

function TCoConnection.OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant): _Recordset;
begin
  Result := DefaultInterface.OpenSchema(Schema, Restrictions, EmptyParam);
end;

function TCoConnection.OpenSchema(Schema: SchemaEnum; Restrictions: OleVariant; SchemaID: OleVariant): _Recordset;
begin
  Result := DefaultInterface.OpenSchema(Schema, Restrictions, SchemaID);
end;

procedure TCoConnection.Cancel;
begin
  DefaultInterface.Cancel;
end;


class function CoRecord_.Create: _Record;
begin
  Result := CreateComObject(CLASS_Record_) as _Record;
end;

class function CoRecord_.CreateRemote(const MachineName: string): _Record;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Record_) as _Record;
end;

procedure TCoRecord.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00000560-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{00000562-0000-0010-8000-00AA006D2EA4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoRecord.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as _Record;
  end;
end;

procedure TCoRecord.ConnectTo(svrIntf: _Record);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCoRecord.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCoRecord.GetDefaultInterface: _Record;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoRecord.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoRecord.Destroy;
begin
  inherited Destroy;
end;

function TCoRecord.Get_ActiveConnection: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.ActiveConnection;
end;

procedure TCoRecord.Set_ActiveConnection(const pvar: WideString);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := pvar;
end;

procedure TCoRecord._Set_ActiveConnection(const pvar: _Connection);
  { Warning: The property ActiveConnection has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ActiveConnection := pvar;
end;

function TCoRecord.Get_State: ObjectStateEnum;
begin
    Result := DefaultInterface.State;
end;

function TCoRecord.Get_Source: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Source;
end;

procedure TCoRecord.Set_Source(const pvar: WideString);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvar;
end;

procedure TCoRecord._Set_Source(const pvar: IDispatch);
  { Warning: The property Source has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Source := pvar;
end;

function TCoRecord.Get_Mode: ConnectModeEnum;
begin
    Result := DefaultInterface.Mode;
end;

procedure TCoRecord.Set_Mode(pMode: ConnectModeEnum);
begin
  DefaultInterface.Set_Mode(pMode);
end;

function TCoRecord.Get_ParentURL: WideString;
begin
    Result := DefaultInterface.ParentURL;
end;

function TCoRecord.Get_Fields: Fields;
begin
    Result := DefaultInterface.Fields;
end;

function TCoRecord.Get_RecordType: RecordTypeEnum;
begin
    Result := DefaultInterface.RecordType;
end;

function TCoRecord.MoveRecord(const Source: WideString; const Destination: WideString; 
                              const UserName: WideString; const Password: WideString; 
                              Options: MoveRecordOptionsEnum; Async: WordBool): WideString;
begin
  Result := DefaultInterface.MoveRecord(Source, Destination, UserName, Password, Options, Async);
end;

function TCoRecord.CopyRecord(const Source: WideString; const Destination: WideString; 
                              const UserName: WideString; const Password: WideString; 
                              Options: CopyRecordOptionsEnum; Async: WordBool): WideString;
begin
  Result := DefaultInterface.CopyRecord(Source, Destination, UserName, Password, Options, Async);
end;

procedure TCoRecord.DeleteRecord(const Source: WideString; Async: WordBool);
begin
  DefaultInterface.DeleteRecord(Source, Async);
end;

procedure TCoRecord.Open(Source: OleVariant; ActiveConnection: OleVariant; Mode: ConnectModeEnum; 
                         CreateOptions: RecordCreateOptionsEnum; Options: RecordOpenOptionsEnum; 
                         const UserName: WideString; const Password: WideString);
begin
  DefaultInterface.Open(Source, ActiveConnection, Mode, CreateOptions, Options, UserName, Password);
end;

procedure TCoRecord.Close;
begin
  DefaultInterface.Close;
end;

function TCoRecord.GetChildren: _Recordset;
begin
  Result := DefaultInterface.GetChildren;
end;

procedure TCoRecord.Cancel;
begin
  DefaultInterface.Cancel;
end;


class function CoStream.Create: _Stream;
begin
  Result := CreateComObject(CLASS_Stream) as _Stream;
end;

class function CoStream.CreateRemote(const MachineName: string): _Stream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Stream) as _Stream;
end;

procedure TCoStream.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00000566-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{00000565-0000-0010-8000-00AA006D2EA4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoStream.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as _Stream;
  end;
end;

procedure TCoStream.ConnectTo(svrIntf: _Stream);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCoStream.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCoStream.GetDefaultInterface: _Stream;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoStream.Destroy;
begin
  inherited Destroy;
end;

function TCoStream.Get_Size: ADO_LONGPTR;
begin
    Result := DefaultInterface.Size;
end;

function TCoStream.Get_EOS: WordBool;
begin
    Result := DefaultInterface.EOS;
end;

function TCoStream.Get_Position: ADO_LONGPTR;
begin
    Result := DefaultInterface.Position;
end;

procedure TCoStream.Set_Position(pPos: ADO_LONGPTR);
begin
  DefaultInterface.Set_Position(pPos);
end;

function TCoStream.Get_type_: StreamTypeEnum;
begin
    Result := DefaultInterface.type_;
end;

procedure TCoStream.Set_type_(ptype: StreamTypeEnum);
begin
  DefaultInterface.Set_type_(ptype);
end;

function TCoStream.Get_LineSeparator: LineSeparatorEnum;
begin
    Result := DefaultInterface.LineSeparator;
end;

procedure TCoStream.Set_LineSeparator(pLS: LineSeparatorEnum);
begin
  DefaultInterface.Set_LineSeparator(pLS);
end;

function TCoStream.Get_State: ObjectStateEnum;
begin
    Result := DefaultInterface.State;
end;

function TCoStream.Get_Mode: ConnectModeEnum;
begin
    Result := DefaultInterface.Mode;
end;

procedure TCoStream.Set_Mode(pMode: ConnectModeEnum);
begin
  DefaultInterface.Set_Mode(pMode);
end;

function TCoStream.Get_Charset: WideString;
begin
    Result := DefaultInterface.Charset;
end;

procedure TCoStream.Set_Charset(const pbstrCharset: WideString);
  { Warning: The property Charset has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Charset := pbstrCharset;
end;

function TCoStream.Read(NumBytes: Integer): OleVariant;
begin
  Result := DefaultInterface.Read(NumBytes);
end;

procedure TCoStream.Open(Source: OleVariant; Mode: ConnectModeEnum; Options: StreamOpenOptionsEnum; 
                         const UserName: WideString; const Password: WideString);
begin
  DefaultInterface.Open(Source, Mode, Options, UserName, Password);
end;

procedure TCoStream.Close;
begin
  DefaultInterface.Close;
end;

procedure TCoStream.SkipLine;
begin
  DefaultInterface.SkipLine;
end;

procedure TCoStream.Write(Buffer: OleVariant);
begin
  DefaultInterface.Write(Buffer);
end;

procedure TCoStream.SetEOS;
begin
  DefaultInterface.SetEOS;
end;

procedure TCoStream.CopyTo(const DestStream: _Stream; CharNumber: ADO_LONGPTR);
begin
  DefaultInterface.CopyTo(DestStream, CharNumber);
end;

procedure TCoStream.Flush;
begin
  DefaultInterface.Flush;
end;

procedure TCoStream.SaveToFile(const FileName: WideString; Options: SaveOptionsEnum);
begin
  DefaultInterface.SaveToFile(FileName, Options);
end;

procedure TCoStream.LoadFromFile(const FileName: WideString);
begin
  DefaultInterface.LoadFromFile(FileName);
end;

function TCoStream.ReadText(NumChars: Integer): WideString;
begin
  Result := DefaultInterface.ReadText(NumChars);
end;

procedure TCoStream.WriteText(const Data: WideString; Options: StreamWriteEnum);
begin
  DefaultInterface.WriteText(Data, Options);
end;

procedure TCoStream.Cancel;
begin
  DefaultInterface.Cancel;
end;


class function CoCommand.Create: _Command;
begin
  Result := CreateComObject(CLASS_Command) as _Command;
end;

class function CoCommand.CreateRemote(const MachineName: string): _Command;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Command) as _Command;
end;

procedure TCoCommand.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00000507-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{B08400BD-F9D1-4D02-B856-71D5DBA123E9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoCommand.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as _Command;
  end;
end;

procedure TCoCommand.ConnectTo(svrIntf: _Command);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCoCommand.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCoCommand.GetDefaultInterface: _Command;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TCoCommand._Set_CommandStream(const pvStream: IUnknown);
  { Warning: The property CommandStream has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.CommandStream := pvStream;
end;

function TCoCommand.Get_CommandStream: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.CommandStream;
end;

procedure TCoCommand.Set_Dialect(const pbstrDialect: WideString);
  { Warning: The property Dialect has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Dialect := pbstrDialect;
end;

function TCoCommand.Get_Dialect: WideString;
begin
    Result := DefaultInterface.Dialect;
end;

procedure TCoCommand.Set_NamedParameters(pfNamedParameters: WordBool);
begin
  DefaultInterface.Set_NamedParameters(pfNamedParameters);
end;

function TCoCommand.Get_NamedParameters: WordBool;
begin
    Result := DefaultInterface.NamedParameters;
end;


class function CoRecordset.Create: _Recordset;
begin
  Result := CreateComObject(CLASS_Recordset) as _Recordset;
end;

class function CoRecordset.CreateRemote(const MachineName: string): _Recordset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recordset) as _Recordset;
end;

procedure TCoRecordset.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00000535-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{00000556-0000-0010-8000-00AA006D2EA4}';
    EventIID:  '{00000266-0000-0010-8000-00AA006D2EA4}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoRecordset.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as _Recordset;
  end;
end;

procedure TCoRecordset.ConnectTo(svrIntf: _Recordset);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCoRecordset.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCoRecordset.GetDefaultInterface: _Recordset;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoRecordset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoRecordset.Destroy;
begin
  inherited Destroy;
end;

procedure TCoRecordset.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    9: if Assigned(FOnWillChangeField) then
         FOnWillChangeField(Self,
                            Params[0] {Integer},
                            Params[1] {OleVariant},
                            EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                            IUnknown(TVarData(Params[3]).VPointer) as _Recordset {const _Recordset});
    10: if Assigned(FOnFieldChangeComplete) then
         FOnFieldChangeComplete(Self,
                                Params[0] {Integer},
                                Params[1] {OleVariant},
                                IUnknown(TVarData(Params[2]).VPointer) as Error {const Error},
                                EventStatusEnum((TVarData(Params[3]).VPointer)^) {var EventStatusEnum},
                                IUnknown(TVarData(Params[4]).VPointer) as _Recordset {const _Recordset});
    11: if Assigned(FOnWillChangeRecord) then
         FOnWillChangeRecord(Self,
                             Params[0] {EventReasonEnum},
                             Params[1] {Integer},
                             EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                             IUnknown(TVarData(Params[3]).VPointer) as _Recordset {const _Recordset});
    12: if Assigned(FOnRecordChangeComplete) then
         FOnRecordChangeComplete(Self,
                                 Params[0] {EventReasonEnum},
                                 Params[1] {Integer},
                                 IUnknown(TVarData(Params[2]).VPointer) as Error {const Error},
                                 EventStatusEnum((TVarData(Params[3]).VPointer)^) {var EventStatusEnum},
                                 IUnknown(TVarData(Params[4]).VPointer) as _Recordset {const _Recordset});
    13: if Assigned(FOnWillChangeRecordset) then
         FOnWillChangeRecordset(Self,
                                Params[0] {EventReasonEnum},
                                EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                                IUnknown(TVarData(Params[2]).VPointer) as _Recordset {const _Recordset});
    14: if Assigned(FOnRecordsetChangeComplete) then
         FOnRecordsetChangeComplete(Self,
                                    Params[0] {EventReasonEnum},
                                    IUnknown(TVarData(Params[1]).VPointer) as Error {const Error},
                                    EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                                    IUnknown(TVarData(Params[3]).VPointer) as _Recordset {const _Recordset});
    15: if Assigned(FOnWillMove) then
         FOnWillMove(Self,
                     Params[0] {EventReasonEnum},
                     EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                     IUnknown(TVarData(Params[2]).VPointer) as _Recordset {const _Recordset});
    16: if Assigned(FOnMoveComplete) then
         FOnMoveComplete(Self,
                         Params[0] {EventReasonEnum},
                         IUnknown(TVarData(Params[1]).VPointer) as Error {const Error},
                         EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                         IUnknown(TVarData(Params[3]).VPointer) as _Recordset {const _Recordset});
    17: if Assigned(FOnEndOfRecordset) then
         FOnEndOfRecordset(Self,
                           WordBool((TVarData(Params[0]).VPointer)^) {var WordBool},
                           EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                           IUnknown(TVarData(Params[2]).VPointer) as _Recordset {const _Recordset});
    18: if Assigned(FOnFetchProgress) then
         FOnFetchProgress(Self,
                          Params[0] {Integer},
                          Params[1] {Integer},
                          EventStatusEnum((TVarData(Params[2]).VPointer)^) {var EventStatusEnum},
                          IUnknown(TVarData(Params[3]).VPointer) as _Recordset {const _Recordset});
    19: if Assigned(FOnFetchComplete) then
         FOnFetchComplete(Self,
                          IUnknown(TVarData(Params[0]).VPointer) as Error {const Error},
                          EventStatusEnum((TVarData(Params[1]).VPointer)^) {var EventStatusEnum},
                          IUnknown(TVarData(Params[2]).VPointer) as _Recordset {const _Recordset});
  end; {case DispID}
end;

procedure TCoRecordset.Save(Destination: OleVariant; PersistFormat: PersistFormatEnum);
begin
  DefaultInterface.Save(Destination, PersistFormat);
end;

class function CoParameter.Create: _Parameter;
begin
  Result := CreateComObject(CLASS_Parameter) as _Parameter;
end;

class function CoParameter.CreateRemote(const MachineName: string): _Parameter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Parameter) as _Parameter;
end;

procedure TCoParameter.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0000050B-0000-0010-8000-00AA006D2EA4}';
    IntfIID:   '{0000050C-0000-0010-8000-00AA006D2EA4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCoParameter.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as _Parameter;
  end;
end;

procedure TCoParameter.ConnectTo(svrIntf: _Parameter);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCoParameter.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCoParameter.GetDefaultInterface: _Parameter;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCoParameter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCoParameter.Destroy;
begin
  inherited Destroy;
end;

function TCoParameter.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TCoParameter.Set_Name(const pbstr: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := pbstr;
end;

function TCoParameter.Get_Value: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Value;
end;

procedure TCoParameter.Set_Value(pvar: OleVariant);
begin
  DefaultInterface.Set_Value(pvar);
end;

function TCoParameter.Get_type_: DataTypeEnum;
begin
    Result := DefaultInterface.type_;
end;

procedure TCoParameter.Set_type_(psDataType: DataTypeEnum);
begin
  DefaultInterface.Set_type_(psDataType);
end;

procedure TCoParameter.Set_Direction(plParmDirection: ParameterDirectionEnum);
begin
  DefaultInterface.Set_Direction(plParmDirection);
end;

function TCoParameter.Get_Direction: ParameterDirectionEnum;
begin
    Result := DefaultInterface.Direction;
end;

procedure TCoParameter.Set_Precision(pbPrecision: Byte);
begin
  DefaultInterface.Set_Precision(pbPrecision);
end;

function TCoParameter.Get_Precision: Byte;
begin
    Result := DefaultInterface.Precision;
end;

procedure TCoParameter.Set_NumericScale(pbScale: Byte);
begin
  DefaultInterface.Set_NumericScale(pbScale);
end;

function TCoParameter.Get_NumericScale: Byte;
begin
    Result := DefaultInterface.NumericScale;
end;

procedure TCoParameter.Set_Size(pl: ADO_LONGPTR);
begin
  DefaultInterface.Set_Size(pl);
end;

function TCoParameter.Get_Size: ADO_LONGPTR;
begin
    Result := DefaultInterface.Size;
end;

function TCoParameter.Get_Attributes: Integer;
begin
    Result := DefaultInterface.Attributes;
end;

procedure TCoParameter.Set_Attributes(plParmAttribs: Integer);
begin
  DefaultInterface.Set_Attributes(plParmAttribs);
end;

procedure TCoParameter.AppendChunk(Val: OleVariant);
begin
  DefaultInterface.AppendChunk(Val);
end;



end.
