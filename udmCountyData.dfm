object dmCountyData: TdmCountyData
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 563
  Width = 794
  PixelsPerInch = 144
  object Connection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 108
    Top = 84
  end
  object Query: TFDQuery
    Connection = Connection
    Left = 252
    Top = 84
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 120
    Top = 204
  end
end
