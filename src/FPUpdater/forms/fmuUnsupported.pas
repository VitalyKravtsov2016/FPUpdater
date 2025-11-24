unit fmuUnsupported;

interface

uses
  Winapi.ShellAPI, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.ExtCtrls, System.Classes,
  Vcl.StdCtrls, System.SysUtils, System.Variants, Vcl.Graphics,
  Vcl.Forms, Vcl.Dialogs;

type
  TfmUnsupported = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Button1: TButton;
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmUnsupported: TfmUnsupported;

implementation

{$R *.dfm}

procedure TfmUnsupported.Label2Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(),'open','www.torgbalance.com',
    nil, nil, SW_SHOWNORMAL);
end;

procedure TfmUnsupported.Label2MouseEnter(Sender: TObject);
begin
  Label2.Font.Style:=[TFontStyle.fsUnderline];
end;

procedure TfmUnsupported.Label2MouseLeave(Sender: TObject);
begin
  Label2.Font.Style:=[];
end;

end.
