object dmCountyData: TdmCountyData
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 375
  Width = 529
  object Connection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 72
    Top = 56
  end
  object Query: TFDQuery
    Connection = Connection
    Left = 168
    Top = 56
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 80
    Top = 136
  end
end
