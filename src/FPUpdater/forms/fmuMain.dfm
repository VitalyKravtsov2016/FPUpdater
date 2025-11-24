object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'fmMain'
  ClientHeight = 263
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblTime: TLabel
    Left = 8
    Top = 190
    Width = 417
    Height = 13
    AutoSize = False
  end
  object btnProperties: TButton
    Left = 431
    Top = 120
    Width = 130
    Height = 41
    Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072
    TabOrder = 0
    OnClick = btnPropertiesClick
  end
  object btnStart: TButton
    Left = 431
    Top = 8
    Width = 130
    Height = 42
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100
    TabOrder = 1
    OnClick = btnStartClick
  end
  object MemoInfo: TMemo
    Left = 8
    Top = 8
    Width = 417
    Height = 153
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 167
    Width = 417
    Height = 17
    TabOrder = 3
  end
  object btnStop: TButton
    Left = 431
    Top = 56
    Width = 130
    Height = 42
    Caption = #1055#1088#1077#1088#1074#1072#1090#1100
    Enabled = False
    TabOrder = 4
    OnClick = btnStopClick
  end
  object MemoStatus: TMemo
    Left = 8
    Top = 209
    Width = 417
    Height = 46
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object btnClose: TButton
    Left = 432
    Top = 216
    Width = 130
    Height = 41
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 6
    OnClick = btnCloseClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 112
  end
end
