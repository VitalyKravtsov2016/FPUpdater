object fmUnsupported: TfmUnsupported
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 181
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    589
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 583
    Height = 61
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 
      #1052#1099' '#1086#1095#1077#1085#1100' '#1089#1086#1078#1072#1083#1077#1077#1084' '#1085#1086' '#1076#1072#1085#1085#1072#1103' '#1084#1086#1076#1077#1083#1100' '#1050#1050#1058' '#1085#1077' '#1087#1086#1076#1076#1077#1088#1078#1080#1074#1072#1077#1090#1089#1103'. '#1045#1089#1083#1080' '#1042 +
      #1099' '#1093#1086#1090#1080#1090#1077' '#1089#1074#1086#1077#1074#1088#1077#1084#1077#1085#1085#1086' '#1087#1086#1083#1091#1095#1072#1090#1100' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1082#1072#1089#1089#1086#1074#1086#1075#1086' '#1055#1054', '#1090#1086' '#1084#1099' '#1073#1091 +
      #1076#1077#1084' '#1088#1072#1076#1099' '#1089#1086#1090#1088#1091#1076#1085#1080#1095#1077#1089#1090#1074#1091'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 111
    Width = 583
    Height = 19
    Align = alTop
    Alignment = taCenter
    Caption = 'www.torgbalance.com'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnClick = Label2Click
    OnMouseEnter = Label2MouseEnter
    OnMouseLeave = Label2MouseLeave
    ExplicitWidth = 158
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Button1: TButton
    Left = 506
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
end
