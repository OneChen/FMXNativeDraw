{------------------------------------------}
{                                          }
{             (c) 2017 by Aone             }
{                                          }
{              QQ: 1467948783              }
{                                          }
{      http://www.cnblogs.com/onechen      }
{                                          }
{------------------------------------------}
{            Start: 2017.06.22             }
{------------------------------------------}

// [原创] 改善 Firemonkey Canvas 几何绘图质量问题（移动平台）by Aone
// http://www.cnblogs.com/onechen/p/6350096.html

unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  ONE.Objects,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Colors;

type
  TForm1 = class(TForm)
    StartAngleTrackBar: TTrackBar;
    EndAngleTrackBar: TTrackBar;
    ThicknessTrackBar: TTrackBar;
    Layout1: TLayout;
    CapFlatRB: TRadioButton;
    CapRoundRB: TRadioButton;
    Layout2: TLayout;
    ColorPanel1: TColorPanel;
    Selection1: TSelection;
    procedure StartAngleTrackBarChange(Sender: TObject);
    procedure EndAngleTrackBarChange(Sender: TObject);
    procedure ThicknessTrackBarChange(Sender: TObject);
    procedure CapFlatRBChange(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Arc1: TOneArc;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ColorPanel1Change(Sender: TObject);
begin
     Arc1.Stroke.Color := ColorPanel1.Color;
end;

procedure TForm1.EndAngleTrackBarChange(Sender: TObject);
begin
     Arc1.EndAngle := EndAngleTrackBar.Value;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     Arc1         := TOneArc.Create(Self);
     Arc1.Align   := TAlignLayout.Client;
     Arc1.HitTest := False;
     Arc1.Margins := TBounds.Create(RectF(10, 10, 10, 10));
     Selection1.AddObject(Arc1);
end;

procedure TForm1.StartAngleTrackBarChange(Sender: TObject);
begin
     Arc1.StartAngle := StartAngleTrackBar.Value;
end;

procedure TForm1.ThicknessTrackBarChange(Sender: TObject);
begin
     Arc1.Stroke.Thickness := ThicknessTrackBar.Value;
end;

procedure TForm1.CapFlatRBChange(Sender: TObject);
begin
     if Sender = CapRoundRB then
          Arc1.Stroke.Cap := TStrokeCap.Round
     else Arc1.Stroke.Cap := TStrokeCap.Flat;
end;

end.
