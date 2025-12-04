object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'fmMain'
  ClientHeight = 264
  ClientWidth = 568
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    568
    264)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTime: TLabel
    Left = 8
    Top = 238
    Width = 418
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object btnProperties: TButton
    Left = 432
    Top = 120
    Width = 130
    Height = 41
    Anchors = [akTop, akRight]
    Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072
    TabOrder = 2
    OnClick = btnPropertiesClick
  end
  object btnStart: TButton
    Left = 432
    Top = 8
    Width = 130
    Height = 42
    Anchors = [akTop, akRight]
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100
    TabOrder = 0
    OnClick = btnStartClick
  end
  object MemoInfo: TMemo
    Left = 8
    Top = 8
    Width = 418
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object btnStop: TButton
    Left = 432
    Top = 56
    Width = 130
    Height = 42
    Anchors = [akTop, akRight]
    Caption = #1055#1088#1077#1088#1074#1072#1090#1100
    Enabled = False
    TabOrder = 1
    OnClick = btnStopClick
  end
  object btnClose: TButton
    Left = 430
    Top = 215
    Width = 130
    Height = 41
    Anchors = [akRight, akBottom]
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object edtStatus: TEdit
    Left = 8
    Top = 215
    Width = 418
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 112
  end
end
