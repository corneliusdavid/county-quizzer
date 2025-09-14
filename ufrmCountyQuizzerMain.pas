unit ufrmCountyQuizzerMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.ListBox,
  FMX.Layouts, FMX.Objects, System.Generics.Collections, System.Generics.Defaults,
  FMX.Memo.Types, FMX.TabControl;

type
  TQuizMode = (qmRecall, qmMultChoice, qmSpelling);
  
  TStateInfo = record
    Name: string;
    CountyCount: Integer;
    Counties: TArray<string>;
  end;
  
  TQuizStats = record
    Correct: Integer;
    Total: Integer;
    function Accuracy: Double;
  end;

  TfrmStateCountyQuiz = class(TForm)
    LayMain: TLayout;
    LayTop: TLayout;
    lblTitle: TLabel;
    cmbStates: TComboBox;
    lblState: TLabel;
    LayQuizMode: TLayout;
    rbRecall: TRadioButton;
    rbRecognition: TRadioButton;
    rbSpelling: TRadioButton;
    LayProgress: TLayout;
    lblProgress: TLabel;
    lblStats: TLabel;
    LayQuiz: TLayout;
    lblQuestion: TLabel;
    memoRecall: TMemo;
    LayMultiChoice: TLayout;
    btnChoice1: TButton;
    btnChoice2: TButton;
    btnChoice3: TButton;
    btnChoice4: TButton;
    edtSpelling: TEdit;
    RectAnswer: TRectangle;
    lblAnswer: TLabel;
    LayButtons: TLayout;
    btnCheck: TButton;
    btnNext: TButton;
    btnReset: TButton;
    lblCountyHint: TLabel;
    StyleBook1: TStyleBook;
    QuizTabs: TTabControl;
    tabRecall: TTabItem;
    tabMultChoice: TTabItem;
    tabSpelling: TTabItem;
    Label1: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure cmbStatesChange(Sender: TObject);
    procedure rbQuizModeChange(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnChoiceClick(Sender: TObject);
    procedure edtSpellingKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure memoRecallChange(Sender: TObject);
  private
    FStates: TDictionary<string, TStateInfo>;
    FCurrentState: TStateInfo;
    FShuffledCounties: TArray<string>;
    FCurrentIndex: Integer;
    FQuizMode: TQuizMode;
    FStats: TQuizStats;
    FAnswered: Boolean;

    procedure InitializeStates;
    procedure LoadStateData(const StateName: string);
    procedure ShuffleCounties;
    procedure UpdateDisplay;
    procedure ShowQuestion;
    procedure ShowAnswer(const UserAnswer: string; IsCorrect: Boolean);
    procedure UpdateStats(IsCorrect: Boolean);
    procedure SetQuizMode(Mode: TQuizMode);
    function GetCurrentCounty: string;
    function GetRandomCounty: string;
    function GenerateMultipleChoice: TArray<string>;
    function CheckRecallAnswer: Integer;
    function CountyMatchFound(const ACounty: string): Boolean;
    procedure ResetQuiz;
    function ScrambleStr(const ACounty: string): string;
    procedure UpdateProgressBar;
    procedure SetupMobileLayout;
    procedure UpdateProgressLabel;
  end;

var
  frmStateCountyQuiz: TfrmStateCountyQuiz;

implementation

{$R *.fmx}

{ TQuizStats }

function TQuizStats.Accuracy: Double;
begin
  if Total = 0 then
    Result := 0.0
  else
    Result := (Correct / Total) * 100.0;
end;

{ TfrmStateCountyQuiz }

procedure TfrmStateCountyQuiz.FormCreate(Sender: TObject);
begin
  FStates := TDictionary<string, TStateInfo>.Create;
  InitializeStates;
  
  // Initialize UI
  FQuizMode := qmRecall;
  FCurrentIndex := 0;
  FAnswered := False;
  
  // Setup mobile-friendly layout
  SetupMobileLayout;
  
  // Load default state (Oregon)
  cmbStates.ItemIndex := cmbStates.Items.IndexOf('Oregon');
  cmbStatesChange(nil);
  
  SetQuizMode(qmRecall);
  UpdateDisplay;
end;

procedure TfrmStateCountyQuiz.FormDestroy(Sender: TObject);
begin
  FStates.Free;
end;

procedure TfrmStateCountyQuiz.FormResize(Sender: TObject);
begin
  SetupMobileLayout;
  UpdateProgressBar;
end;

procedure TfrmStateCountyQuiz.SetupMobileLayout;
begin
  // Adjust button heights for touch
  btnChoice1.Height := 45;
  btnChoice2.Height := 45;
  btnChoice3.Height := 45;
  btnChoice4.Height := 45;

  // Make sure controls are properly spaced for touch
  LayMultiChoice.Padding.Top := 10;
  LayMultiChoice.Padding.Bottom := 10;
  LayButtons.Padding.Top := 15;
end;

procedure TfrmStateCountyQuiz.InitializeStates;
var
  StateInfo: TStateInfo;
begin
  // test (4 counties)
  StateInfo.Name := 'Test';
  StateInfo.CountyCount := 4;
  StateInfo.Counties := TArray<string>.Create('one', 'two', 'three', 'four');
  FStates.Add('Test', StateInfo);

  // Oregon (36 counties)
  StateInfo.Name := 'Oregon';
  StateInfo.CountyCount := 36;
  StateInfo.Counties := TArray<string>.Create(
    'Baker', 'Benton', 'Clackamas', 'Clatsop', 'Columbia', 'Coos', 'Crook', 'Curry',
    'Deschutes', 'Douglas', 'Gilliam', 'Grant', 'Harney', 'Hood River', 'Jackson',
    'Jefferson', 'Josephine', 'Klamath', 'Lake', 'Lane', 'Lincoln', 'Linn', 'Malheur',
    'Marion', 'Morrow', 'Multnomah', 'Polk', 'Sherman', 'Tillamook', 'Umatilla',
    'Union', 'Wallowa', 'Wasco', 'Washington', 'Wheeler', 'Yamhill'
  );
  FStates.Add('Oregon', StateInfo);

  // Washington (39 counties)
  StateInfo.Name := 'Washington';
  StateInfo.CountyCount := 39;
  StateInfo.Counties := TArray<string>.Create(
    'Adams', 'Asotin', 'Benton', 'Chelan', 'Clallam', 'Clark', 'Columbia', 'Cowlitz',
    'Douglas', 'Ferry', 'Franklin', 'Garfield', 'Grant', 'Grays Harbor', 'Island',
    'Jefferson', 'King', 'Kitsap', 'Kittitas', 'Klickitat', 'Lewis', 'Lincoln',
    'Mason', 'Okanogan', 'Pacific', 'Pend Oreille', 'Pierce', 'San Juan', 'Skagit',
    'Skamania', 'Snohomish', 'Spokane', 'Stevens', 'Thurston', 'Wahkiakum', 'Walla Walla',
    'Whatcom', 'Whitman', 'Yakima'
  );
  FStates.Add('Washington', StateInfo);

  // California (58 counties)
  StateInfo.Name := 'California';
  StateInfo.CountyCount := 58;
  StateInfo.Counties := TArray<string>.Create(
    'Alameda', 'Alpine', 'Amador', 'Butte', 'Calaveras', 'Colusa', 'Contra Costa',
    'Del Norte', 'El Dorado', 'Fresno', 'Glenn', 'Humboldt', 'Imperial', 'Inyo',
    'Kern', 'Kings', 'Lake', 'Lassen', 'Los Angeles', 'Madera', 'Marin', 'Mariposa',
    'Mendocino', 'Merced', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Orange',
    'Placer', 'Plumas', 'Riverside', 'Sacramento', 'San Benito', 'San Bernardino',
    'San Diego', 'San Francisco', 'San Joaquin', 'San Luis Obispo', 'San Mateo',
    'Santa Barbara', 'Santa Clara', 'Santa Cruz', 'Shasta', 'Sierra', 'Siskiyou',
    'Solano', 'Sonoma', 'Stanislaus', 'Sutter', 'Tehama', 'Trinity', 'Tulare',
    'Tuolumne', 'Ventura', 'Yolo', 'Yuba'
  );
  FStates.Add('California', StateInfo);

  // Texas (254 counties) - Adding larger subset for more challenge
  StateInfo.Name := 'Texas';
  StateInfo.CountyCount := 50; // Subset for demo
  StateInfo.Counties := TArray<string>.Create(
    'Harris', 'Dallas', 'Tarrant', 'Bexar', 'Travis', 'Collin', 'Denton', 'Fort Bend',
    'Hidalgo', 'El Paso', 'Williamson', 'Montgomery', 'Galveston', 'Brazoria',
    'Jefferson', 'Nueces', 'Bell', 'Cameron', 'Brazos', 'Hays', 'McLennan', 'Guadalupe',
    'Ellis', 'Johnson', 'Kaufman', 'Parker', 'Rockwall', 'Smith', 'Webb', 'Lubbock',
    'Jefferson', 'Orange', 'Chambers', 'Liberty', 'Waller', 'Bastrop', 'Caldwell',
    'Comal', 'Kendall', 'Bandera', 'Medina', 'Uvalde', 'Kerr', 'Gillespie', 'Blanco',
    'Burnet', 'Llano', 'Mason', 'Kimble', 'Menard'
  );
  FStates.Add('Texas', StateInfo);

  // Florida (67 counties)
  StateInfo.Name := 'Florida';
  StateInfo.CountyCount := 25; // Subset
  StateInfo.Counties := TArray<string>.Create(
    'Miami-Dade', 'Broward', 'Palm Beach', 'Hillsborough', 'Orange', 'Pinellas',
    'Duval', 'Lee', 'Polk', 'Volusia', 'Brevard', 'Pasco', 'Seminole', 'Sarasota',
    'Manatee', 'Lake', 'Collier', 'Leon', 'Escambia', 'Clay', 'St. Johns',
    'Osceola', 'Marion', 'Alachua', 'Martin'
  );
  FStates.Add('Florida', StateInfo);

  // New York (62 counties)
  StateInfo.Name := 'New York';
  StateInfo.CountyCount := 30; // Subset
  StateInfo.Counties := TArray<string>.Create(
    'Kings', 'Queens', 'New York', 'Suffolk', 'Bronx', 'Nassau', 'Westchester',
    'Erie', 'Monroe', 'Richmond', 'Onondaga', 'Orange', 'Rockland', 'Albany',
    'Oneida', 'Niagara', 'Dutchess', 'Saratoga', 'Rensselaer', 'Broome', 'Jefferson',
    'St. Lawrence', 'Chautauqua', 'Oswego', 'Ontario', 'Washington', 'Ulster',
    'Putnam', 'Wayne', 'Cayuga'
  );
  FStates.Add('New York', StateInfo);

  // Populate combo box
  for var State in FStates.Keys do
    cmbStates.Items.Add(State);
end;

procedure TfrmStateCountyQuiz.LoadStateData(const StateName: string);
begin
  if FStates.TryGetValue(StateName, FCurrentState) then
  begin
    lblTitle.Text := Format('%s Counties Quiz', [StateName]);
    ShuffleCounties;
    ResetQuiz;
  end;
end;

procedure TfrmStateCountyQuiz.memoRecallChange(Sender: TObject);
begin
  FCurrentIndex := if memoRecall.Lines.Count > 0 then memoRecall.Lines.Count - 1 else 0;
  UpdateProgressLabel;
  UpdateProgressBar;
end;

procedure TfrmStateCountyQuiz.ShuffleCounties;
begin
  // Copy counties array
  SetLength(FShuffledCounties, Length(FCurrentState.Counties));
  for var i := 0 to High(FCurrentState.Counties) do
    FShuffledCounties[i] := FCurrentState.Counties[i];

  // Fisher-Yates shuffle
  for var i := High(FShuffledCounties) downto 1 do
  begin
    var j := Random(i + 1);
    var temp := FShuffledCounties[i];
    FShuffledCounties[i] := FShuffledCounties[j];
    FShuffledCounties[j] := temp;
  end;
end;

procedure TfrmStateCountyQuiz.UpdateDisplay;
begin
  // Update progress
  if Length(FShuffledCounties) > 0 then
  begin
    UpdateProgressLabel;
    UpdateProgressBar;
  end;

  // Update stats
  lblStats.Text := Format('Score: %d/%d (%.1f%%)',
    [FStats.Correct, FStats.Total, FStats.Accuracy]);

  // Show current question
  ShowQuestion;
end;

procedure TfrmStateCountyQuiz.UpdateProgressBar;
begin
  ProgressBar.Value := FCurrentIndex;
end;

procedure TfrmStateCountyQuiz.ShowQuestion;
var
  Choices: TArray<string>;
begin
  rectAnswer.Visible := False;

  FAnswered := False;
  btnNext.Enabled := False;
  btnCheck.Enabled := True;

  case FQuizMode of
    qmRecall:
    begin
      lblQuestion.Text := Format('Name all %d counties in %s:',
        [FCurrentState.CountyCount, FCurrentState.Name]);
      memoRecall.Visible := True;
      memoRecall.Lines.Clear;
      memoRecall.SetFocus;
    end;

    qmMultChoice:
    begin
      if FCurrentIndex < Length(FShuffledCounties) then
      begin
        lblQuestion.Text := Format('County #%d - Which county is in %s?',
          [FCurrentIndex + 1, FCurrentState.Name]);
        Choices := GenerateMultipleChoice;
        btnChoice1.Text := Choices[0];
        btnChoice2.Text := Choices[1];
        btnChoice3.Text := Choices[2];
        btnChoice4.Text := Choices[3];
        LayMultiChoice.Visible := True;
        btnCheck.Enabled := False; // Answer immediately on click
      end;
    end;

    qmSpelling:
    begin
      if FCurrentIndex < Length(FShuffledCounties) then
      begin
        lblQuestion.Text := Format('County #%d - Type the correct name:', [FCurrentIndex + 1]);
        lblCountyHint.Text := ScrambleStr(GetCurrentCounty);
        lblCountyHint.Visible := True;
        edtSpelling.Text := EmptyStr;
        edtSpelling.SetFocus;
      end;
    end;
  end;
end;

procedure TfrmStateCountyQuiz.ShowAnswer(const UserAnswer: string; IsCorrect: Boolean);
begin
  FAnswered := True;
  btnCheck.Enabled := False;

  if FQuizMode = qmRecall then
    // ShowAnswer is only called when completed
    lblAnswer.Text := Format('Completed! You got %d out of %d counties.',
          [FStats.Correct, FCurrentState.CountyCount])
  else begin
    if IsCorrect then begin
      lblAnswer.Text := '✓ Correct!';
      RectAnswer.Fill.Color := TAlphaColorRec.Lightgreen;
    end else begin
      lblAnswer.Text := Format('✗ Incorrect. The answer is: %s', [GetCurrentCounty]);
      RectAnswer.Fill.Color := TAlphaColorRec.Lightcoral;
    end;

    // Enable next button for non-recall modes
    if FQuizMode <> qmRecall then
        btnNext.Enabled := True;
  end;

  rectAnswer.Visible := True;
end;

procedure TfrmStateCountyQuiz.UpdateStats(IsCorrect: Boolean);
begin
  Inc(FStats.Total);
  if IsCorrect then
    Inc(FStats.Correct);
end;

procedure TfrmStateCountyQuiz.SetQuizMode(Mode: TQuizMode);
begin
  FQuizMode := Mode;
  case Mode of
    qmRecall:
      begin
         rbRecall.IsChecked := True;
         QuizTabs.ActiveTab := tabRecall;
      end;
    qmMultChoice:
      begin
        rbRecognition.IsChecked := True;
        QuizTabs.ActiveTab := tabMultChoice;
      end;
    qmSpelling:
      begin
        rbSpelling.IsChecked := True;
        QuizTabs.ActiveTab := tabSpelling;
      end;
  end;

  FCurrentIndex := 0;
  FAnswered := False;
  UpdateDisplay;
end;

function TfrmStateCountyQuiz.GetCurrentCounty: string;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < Length(FShuffledCounties)) then
    Result := FShuffledCounties[FCurrentIndex]
  else
    Result := '';
end;

function TfrmStateCountyQuiz.GetRandomCounty: string;
var
  ACounty: string;
  RandomStateIdx: Integer;
  RandomCountyIdx: Integer;
  StateList: TArray<string>;
begin
  // find random state not the current one
  StateList := FStates.Keys.ToArray;
  repeat
    RandomStateIdx := Random(Length(StateList));
  until StateList[RandomStateIdx] <> FCurrentState.Name;

  repeat
    // get a random county from the selected random state
    RandomCountyIdx := Random(Length(FStates[StateList[RandomStateIdx]].Counties));
    ACounty := FStates[StateList[RandomStateIdx]].Counties[RandomCountyIdx];
  until not CountyMatchFound(ACounty);

  Result := ACounty;
end;

function TfrmStateCountyQuiz.GenerateMultipleChoice: TArray<string>;
var
  Correct: string;
  Options: TList<string>;
  i, RandomIndex: Integer;
begin
  Correct := GetCurrentCounty;
  Options := TList<string>.Create;
  try
    Options.Add(Correct);

    // Add 3 random incorrect options from other states
    while Options.Count < 4 do
    begin
      Options.Add(GetRandomCounty);
    end;

    // Shuffle the options
    for i := Options.Count - 1 downto 1 do
    begin
      RandomIndex := Random(i + 1);
      Options.Exchange(i, RandomIndex);
    end;

    Result := Options.ToArray;
  finally
    Options.Free;
  end;
end;

function TfrmStateCountyQuiz.CheckRecallAnswer: Integer;
var
  UserCounties: TArray<string>;
  UserList: TStringList;
  i: Integer;
  County: string;
begin
  Result := 0;
  UserList := TStringList.Create;
  try
    UserList.Text := memoRecall.Lines.Text;

    // Clean up user input
    for i := UserList.Count - 1 downto 0 do
    begin
      County := Trim(UserList[i]);
      if County.IsEmpty then
        UserList.Delete(i)
      else
        UserList[i] := County;
    end;

    // Count correct matches
    for County in FCurrentState.Counties do
    begin
      for i := 0 to UserList.Count - 1 do
      begin
        if SameText(UserList[i], County) then
        begin
          Inc(Result);
          Break;
        end;
      end;
    end;
  finally
    UserList.Free;
  end;
end;

procedure TfrmStateCountyQuiz.ResetQuiz;
begin
  FStats.Correct := 0;
  FStats.Total := 0;
  FCurrentIndex := 0;
  FAnswered := False;
  ProgressBar.Max := Length(FShuffledCounties);
  UpdateDisplay;
end;

// Event Handlers

procedure TfrmStateCountyQuiz.cmbStatesChange(Sender: TObject);
begin
  if cmbStates.ItemIndex >= 0 then
    LoadStateData(cmbStates.Selected.Text);
end;

procedure TfrmStateCountyQuiz.rbQuizModeChange(Sender: TObject);
begin
  if rbRecall.IsChecked then
    SetQuizMode(qmRecall)
  else if rbRecognition.IsChecked then
    SetQuizMode(qmMultChoice)
  else if rbSpelling.IsChecked then
    SetQuizMode(qmSpelling);
end;

procedure TfrmStateCountyQuiz.btnCheckClick(Sender: TObject);
var
  IsCorrect: Boolean;
  CorrectCount: Integer;
begin
  if FAnswered then Exit;

  case FQuizMode of
    qmRecall:
    begin
      CorrectCount := CheckRecallAnswer;
      FStats.Correct := CorrectCount;
      FStats.Total := FCurrentState.CountyCount;
      ShowAnswer('', False); // Always show as "completed"
    end;

    qmSpelling:
    begin
      IsCorrect := SameText(Trim(edtSpelling.Text), GetCurrentCounty);
      UpdateStats(IsCorrect);
      ShowAnswer(edtSpelling.Text, IsCorrect);
    end;
  end;
end;

procedure TfrmStateCountyQuiz.btnNextClick(Sender: TObject);
begin
  if FCurrentIndex < Length(FShuffledCounties) - 1 then
  begin
    Inc(FCurrentIndex);
    UpdateDisplay;
  end;
end;

procedure TfrmStateCountyQuiz.btnResetClick(Sender: TObject);
begin
  ResetQuiz;
end;

procedure TfrmStateCountyQuiz.btnChoiceClick(Sender: TObject);
var
  SelectedCounty: string;
  IsCorrect: Boolean;
begin
  if FAnswered then Exit;

  SelectedCounty := (Sender as TButton).Text;
  IsCorrect := SameText(SelectedCounty, GetCurrentCounty);
  UpdateStats(IsCorrect);
  ShowAnswer(SelectedCounty, IsCorrect);
end;

function TfrmStateCountyQuiz.CountyMatchFound(const ACounty: string): Boolean;
begin
  Result := False;

  for var c in FCurrentState.Counties do
    if SameText(c, ACounty) then begin
      Result := True;
      Break;
    end;
end;

procedure TfrmStateCountyQuiz.edtSpellingKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and not FAnswered then // Enter key
  begin
    btnCheckClick(Sender);
  end;
end;

function TfrmStateCountyQuiz.ScrambleStr(const ACounty: string): string;
var
  cLen: Integer;
  p1, p2: Integer;
begin
  Result := UpperCase(ACounty);

  cLen := ACounty.Length;
  for var ScrambleCount := 1 to 50 do begin
     p1 := Random(cLen) + 1;
     repeat
       p2 := Random(cLen) + 1;
     until p1 <> p2;

     var Tmp := Result[p1];
     Result[p1] := Result[p2];
     Result[p2] := Tmp;
  end;
end;

procedure TfrmStateCountyQuiz.UpdateProgressLabel;
begin
  lblProgress.Text := Format('Progress: %d/%d', [FCurrentIndex, Length(FShuffledCounties)]);
end;


end.