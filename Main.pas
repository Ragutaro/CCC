unit Main;
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.StrUtils, IniFilesDX, System.IOUtils, System.Types,
  Vcl.Filectrl, Vcl.ExtCtrls, DateUtils, Vcl.Menus;

type
  TfrmMain = class(TForm)
    Timer: TTimer;
    TrayIcon: TTrayIcon;
    pop: TPopupMenu;
    popClose: TMenuItem;
    popHistory: TMenuItem;
    popChangeNow: TMenuItem;
    N1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TimerTimer(Sender: TObject);
    procedure popCloseClick(Sender: TObject);
    procedure popHistoryClick(Sender: TObject);
    procedure popChangeNowClick(Sender: TObject);
  private
    { Private 宣言 }
    procedure _LoadSettings;
    procedure _SaveSettings;
    procedure _ChangeAccentColor;
  public
    { Public 宣言 }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  HideUtils,
  Colors,
  History,
  dp;

procedure TfrmMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Application.ShowMainForm := False;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  _SaveSettings;
  Release;
  frmMain := nil;   //フォーム名に変更する
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if IsDebugMode then
     Self.Caption := 'Debug Mode - ' + Self.Caption;
  DisableVclStyles(Self, '');
  _LoadSettings;
end;

procedure TfrmMain._ChangeAccentColor;
var
  sl : TStringList;
  cColor : TColor;
  sFile : String;
begin
  cColor := CreateRandomColor;
  WriteAccentColor(cColor);
  sl := TStringList.Create;
  try
    sFile := GetApplicationPath + 'History.txt';
    if FileExists(sFile) then
      sl.LoadFromFile(sFile, TEncoding.UTF8);
    sl.Add(ColorToRGBString(cColor));
    sl.SaveToFile(sFile, TEncoding.UTF8);
  finally
    sl.Free;
  end;
end;

procedure TfrmMain._LoadSettings;
var
  ini : TMemIniFile;
begin
  ini := TMemIniFile.Create(GetIniFileName, TEncoding.UTF8);
  try
    ini.ReadWindowPosition(Self.Name, Self);
    Self.Font.Name := ini.ReadString('General', 'FontName', 'メイリオ');
    Self.Font.Size := ini.ReadInteger('General', 'FontSize', 9);
  finally
    ini.Free;
  end;
end;

procedure TfrmMain._SaveSettings;
var
  ini : TMemIniFile;
begin
  ini := TMemIniFile.Create(GetIniFileName, TEncoding.UTF8);
  try
    ini.WriteWindowPosition(Self.Name, Self);
    ini.WriteString('General', 'FontName', Self.Font.Name);
    ini.WriteInteger('General', 'FontSize', Self.Font.Size);
  finally
    ini.UpdateFile;
    ini.Free;
  end;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    char(VK_ESCAPE) :
      begin
        Key := char(0);
        Close;
      end;
  end;
end;

procedure TfrmMain.popChangeNowClick(Sender: TObject);
begin
  _ChangeAccentColor;
end;

procedure TfrmMain.popCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.popHistoryClick(Sender: TObject);
begin
  Application.CreateForm(TfrmHistory, frmHistory);
  frmHistory.Show;
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
begin
  if (MinuteOf(Now) = 0) and (SecondOf(Now) = 0) then
  begin
    _ChangeAccentColor;
  end;
end;

end.

