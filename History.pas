unit History;
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.StrUtils, IniFilesDX, System.IOUtils, System.Types,
  Vcl.Filectrl, Vcl.StdCtrls;

type
  TfrmHistory = class(TForm)
    lstList: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lstListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
  private
    { Private 宣言 }
    procedure _LoadSettings;
    procedure _SaveSettings;
    procedure _LoadHistory;
  public
    { Public 宣言 }
  end;

var
  frmHistory: TfrmHistory;

implementation

{$R *.dfm}

uses
  HideUtils,
  dp;

procedure TfrmHistory.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  _SaveSettings;
  Release;
  frmHistory := nil;   //フォーム名に変更する
end;

procedure TfrmHistory.FormCreate(Sender: TObject);
begin
  if IsDebugMode then
     Self.Caption := 'Debug Mode - ' + Self.Caption;
  DisableVclStyles(Self, '');
  _LoadSettings;
  _LoadHistory;
end;

procedure TfrmHistory._LoadHistory;
var
  sl, sm : TStringList;
  sFile : String;
  i, r, g, b : Integer;
begin
  sFile := GetApplicationPath + 'History.txt';
  sl := TStringList.Create;
  sm := TStringList.Create;
  try
    if FileExists(sFile) then
      sl.LoadFromFile(sFile, TEncoding.UTF8);

    for i := 0 to sl.Count-1 do
    begin
      sm.CommaText := sl[i];
      r := StrToInt(sm[0]);
      g := StrToInt(sm[1]);
      b := StrToInt(sm[2]);
      lstList.Items.Add(ColorToString(RGB(r, g, b)));
    end;
  finally
    sl.Free;
    sm.Free;
  end;
end;

procedure TfrmHistory._LoadSettings;
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

procedure TfrmHistory._SaveSettings;
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

procedure TfrmHistory.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    char(VK_ESCAPE) :
      begin
        Key := char(0);
        Close;
      end;
  end;
end;

procedure TfrmHistory.FormResize(Sender: TObject);
begin
  lstList.Columns := (Self.Width div 100);
end;

procedure TfrmHistory.lstListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  r : TRect;
  cColor : TColor;
  s : String;
begin
  with lstList.Canvas do
  begin
    FillRect(Rect);
    cColor := StringToColor(lstList.Items[Index]);
    s := Format('#%s%s%s',[IntToHex(GetRValue(cColor), 2),
                           IntToHex(GetGValue(cColor), 2),
                           IntToHex(GetBValue(cColor), 2)]);
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := cColor;
    Brush.Style := bsSolid;
    if odSelected in State then
      Font.Color := clWhite
    else
      Font.Color := clWindowText;
    r.Top := Rect.Top+1;
    r.Left := Rect.Left+2;
    r.Right := r.Left + 29;
    r.Bottom := Rect.Bottom-1;
    Rectangle(r);
    r := Rect;
    r.Left := r.Left + 35;
    SetBkMode(Handle, TRANSPARENT);
    DrawText(Handle, pchar(s), -1, r, DT_WORD_ELLIPSIS);
  end;
end;

end.

