object frmHistory: TfrmHistory
  Left = 326
  Top = 356
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = #33394#12398#23653#27508
  ClientHeight = 200
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #12513#12452#12522#12458
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 18
  object lstList: TListBox
    Left = 0
    Top = 0
    Width = 400
    Height = 200
    Style = lbOwnerDrawFixed
    Align = alClient
    Columns = 4
    TabOrder = 0
    OnDrawItem = lstListDrawItem
  end
end
