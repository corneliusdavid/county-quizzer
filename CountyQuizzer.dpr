program CountyQuizzer;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmCountyQuizzerMain in 'ufrmCountyQuizzerMain.pas' {frmCountyQuizzer},
  udmCountyData in 'udmCountyData.pas' {dmCountyData: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmCountyData, dmCountyData);
  Application.CreateForm(TfrmStateCountyQuiz, frmStateCountyQuiz);
  Application.Run;
end.
