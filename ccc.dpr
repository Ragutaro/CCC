program ccc;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  History in 'History.pas' {frmHistory};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
