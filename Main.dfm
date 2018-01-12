object frmMain: TfrmMain
  Left = 331
  Top = 116
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Change Caption Color'
  ClientHeight = 131
  ClientWidth = 226
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
  WindowState = wsMinimized
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 18
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 56
    Top = 50
  end
  object TrayIcon: TTrayIcon
    PopupMenu = pop
    Visible = True
    Left = 110
    Top = 52
  end
  object pop: TPopupMenu
    Left = 168
    Top = 50
    object popHistory: TMenuItem
      Caption = #33394#12398#23653#27508'...'
      OnClick = popHistoryClick
    end
    object popChangeNow: TMenuItem
      Caption = #20170#12377#12368#12395#22793#26356
      OnClick = popChangeNowClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object popClose: TMenuItem
      Caption = #32066#20102
      OnClick = popCloseClick
    end
  end
end
