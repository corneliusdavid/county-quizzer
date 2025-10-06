unit udmCountyData;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils,
  FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Pool, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet;

type
  TStateInfo = record
    Name: string;
    CountyCount: Integer;
    Counties: TArray<string>;
  end;

  TdmCountyData = class(TDataModule)
    Connection: TFDConnection;
    Query: TFDQuery;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    function GetDatabasePath: string;
    procedure InitializeDatabase;
  public
    function LoadStates: TArray<TStateInfo>;
    function GetStatesList: TArray<string>;
    function GetCountiesForState(const StateName: string): TArray<string>;
    function GetRandomCountyFromOtherStates(const CurrentStateName: string): string;
  end;

var
  dmCountyData: TdmCountyData;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  ufrmCountyQuizzerMain;

{$R *.dfm}

procedure TdmCountyData.DataModuleCreate(Sender: TObject);
begin
  InitializeDatabase;
end;

procedure TdmCountyData.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(Connection) then
    Connection.Connected := False;
end;

function TdmCountyData.GetDatabasePath: string;
begin
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  Result := TPath.Combine(TPath.GetDocumentsPath, 'Counties.db');
  {$ELSE}
  Result := TPath.Combine(TPath.Combine(TPath.GetDocumentsPath, TfrmStateCountyQuiz.APPLICATION_NAME), 'Counties.db');
  {$IFEND}
end;

procedure TdmCountyData.InitializeDatabase;
begin
  Connection.DriverName := 'SQLite';
  Connection.Params.Values['Database'] := GetDatabasePath;
  
  try
    Connection.Connected := True;
  except
    on E: Exception do
    begin
      raise Exception.Create('Database connection failed: ' + E.Message + #13#10 +
                           'Make sure counties.db exists in: ' + GetDatabasePath);
    end;
  end;
end;

function TdmCountyData.LoadStates: TArray<TStateInfo>;
var
  StatesList: TList<TStateInfo>;
  StateInfo: TStateInfo;
  CountiesList: TStringList;
begin
  StatesList := TList<TStateInfo>.Create;
  CountiesList := TStringList.Create;
  try
    Query.SQL.Text := 'SELECT name, county_count FROM states ORDER BY name';
    Query.Open;
    
    while not Query.Eof do
    begin
      StateInfo.Name := Query.FieldByName('name').AsString;
      StateInfo.CountyCount := Query.FieldByName('county_count').AsInteger;
      
      // Load counties for this state
      CountiesList.Clear;
      with TFDQuery.Create(nil) do
      try
        Connection := Self.Connection;
        SQL.Text := 'SELECT c.name FROM counties c ' +
                   'JOIN states s ON c.state_id = s.id ' +
                   'WHERE s.name = :state_name ORDER BY c.name';
        ParamByName('state_name').AsString := StateInfo.Name;
        Open;
        
        while not Eof do
        begin
          CountiesList.Add(FieldByName('name').AsString);
          Next;
        end;
      finally
        Free;
      end;
      
      // Convert to array
      SetLength(StateInfo.Counties, CountiesList.Count);
      for var i := 0 to CountiesList.Count - 1 do
        StateInfo.Counties[i] := CountiesList[i];
      
      StatesList.Add(StateInfo);
      Query.Next;
    end;
    
    Query.Close;
    Result := StatesList.ToArray;
  finally
    CountiesList.Free;
    StatesList.Free;
  end;
end;

function TdmCountyData.GetStatesList: TArray<string>;
var
  StatesList: TStringList;
begin
  StatesList := TStringList.Create;
  try
    Query.SQL.Text := 'SELECT name FROM states ORDER BY name';
    Query.Open;
    
    while not Query.Eof do
    begin
      StatesList.Add(Query.FieldByName('name').AsString);
      Query.Next;
    end;

    Query.Close;
    Result := StatesList.ToStringArray;
  finally
    StatesList.Free;
  end;
end;

function TdmCountyData.GetCountiesForState(const StateName: string): TArray<string>;
var
  CountiesList: TStringList;
begin
  CountiesList := TStringList.Create;
  try
    Query.SQL.Text := 'SELECT c.name FROM counties c ' +
                     'JOIN states s ON c.state_id = s.id ' +
                     'WHERE s.name = :state_name ORDER BY c.name';
    Query.ParamByName('state_name').AsString := StateName;
    Query.Open;
    
    while not Query.Eof do
    begin
      CountiesList.Add(Query.FieldByName('name').AsString);
      Query.Next;
    end;
    
    Query.Close;
    Result := CountiesList.ToStringArray;
  finally
    CountiesList.Free;
  end;
end;

function TdmCountyData.GetRandomCountyFromOtherStates(const CurrentStateName: string): string;
begin
  Query.SQL.Text := '''
  SELECT c.name FROM counties c
  JOIN states s ON c.state_id = s.id
  WHERE s.name <> :current_state ORDER BY RANDOM() LIMIT 1
  ''';
  Query.ParamByName('current_state').AsString := CurrentStateName;
  Query.Open;

  if not Query.Eof then
    Result := Query.FieldByName('name').AsString
  else
    Result := 'Unknown County'; // Fallback

  Query.Close;
end;

end.