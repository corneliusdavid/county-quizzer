program CountyQizzer;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmCountyQuizzerMain in 'ufrmCountyQuizzerMain.pas' {frmCountyQuizzer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmStateCountyQuiz, frmStateCountyQuiz);
  Application.Run;
end.
