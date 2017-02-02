unit Main;

{$i ..\NativeDraw.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Graphics.Native,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.TabControl, FMX.ListBox, FMX.Colors;

type
  TForm1 = class(TForm)
    LineThicknessSpinBox: TSpinBox;
    PaintBox1: TPaintBox;
    Selection1: TSelection;
    PixelRB: TRadioButton;
    PointRB: TRadioButton;
    TabControl1: TTabControl;
    LineTab: TTabItem;
    LineLayout: TLayout;
    ShapeTab: TTabItem;
    Rectangle1: TRectangle;
    ThicknessSpinBox: TTrackBar;
    RadiusSpinBox: TTrackBar;
    CapListBox: TListBox;
    DashListBox: TListBox;
    ShapeListBox: TListBox;
    Layout1: TLayout;
    Layout7: TLayout;
    StrokeColor: TComboColorBox;
    FillColor: TComboColorBox;
    BackRectangle: TRectangle;
    FillColor1: TComboColorBox;
    FillGradientChk: TCheckBox;
    Ellipse1: TEllipse;
    Arc1: TArc;
    Path1: TPath;
    Label1: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    StrokeColor1: TComboColorBox;
    StrokeGradientChk: TCheckBox;
    Label2: TLabel;
    ArcLayout: TLayout;
    Label3: TLabel;
    ArcStartAngleTrackBar: TTrackBar;
    ArcEndAngleTrackBar: TTrackBar;
    procedure LineThicknessSpinBoxChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure ThicknessSpinBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShapeListBoxChange(Sender: TObject);
    procedure CapListBoxChange(Sender: TObject);
    procedure DashListBoxChange(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    procedure Repaint;
    function NowShape: TShape;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormShow(Sender: TObject);
begin
     ShapeListBox.ItemIndex := 0;
     CapListBox.ItemIndex   := 0;
     DashListBox.ItemIndex  := 0;
end;

procedure TForm1.Repaint;
begin
     Self.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var i: Integer;
    SrcRect, DesRect: TRectF;
begin
     SrcRect := PaintBox1.LocalRect;

     {$IFDEF UseNativeDraw}Canvas.NativeDraw(SrcRect{注意:请勿在昵名函数里改变它}, procedure var i: Integer; begin {$ENDIF} // 原生绘图 by Aone, 昵名函数里加入绘图方法, 内部会先画到 Bitmap

     if TabControl1.ActiveTab = LineTab then
     begin
          Canvas.Stroke.Kind := TBrushKind.Solid;
          Canvas.Fill.Kind   := TBrushKind.Solid;
          Canvas.Fill.Color  := TAlphaColorRec.Black;

          for i:=1 to Trunc(SrcRect.Width / 25) - 1 do
          begin
               if PixelRB.IsChecked then
                    Canvas.Stroke.Thickness := LineThicknessSpinBox.Value / Self.Canvas.Scale // 1 Pixel
               else Canvas.Stroke.Thickness := LineThicknessSpinBox.Value;                    // 1 Point
               Canvas.DrawLine(PointF(SrcRect.Left + i * 25, SrcRect.Top), PointF(SrcRect.Left + i * 25, SrcRect.Bottom), 1);
          end;

          Canvas.DrawLine(PointF(SrcRect.Left, SrcRect.Top), PointF(SrcRect.Right, SrcRect.Bottom), 1);
     end
     else
     if TabControl1.ActiveTab = ShapeTab then
     begin
          DesRect := SrcRect;
          InflateRect(DesRect, -(NowShape.Stroke.Thickness / 2), -(NowShape.Stroke.Thickness / 2)); // 线在区内

          case ShapeListBox.ItemIndex of
               0: // Rectangle
                  begin
                       Canvas.FillRect(DesRect, Rectangle1.XRadius, Rectangle1.YRadius, AllCorners, 1, Rectangle1.Fill);
                       Canvas.DrawRect(DesRect, Rectangle1.XRadius, Rectangle1.YRadius, AllCorners, 1, Rectangle1.Stroke);
                  end;
               1: // Ellipse
                  begin
                       Canvas.FillEllipse(DesRect, 1, Ellipse1.Fill);
                       Canvas.DrawEllipse(DesRect, 1, Ellipse1.Stroke);
                  end;
               2: // Arc
                  begin
                       Canvas.FillArc(DesRect.CenterPoint, PointF(DesRect.Width * 0.5, DesRect.Height * 0.5), Arc1.StartAngle, Arc1.EndAngle, 1, Arc1.Fill);
                       Canvas.DrawArc(DesRect.CenterPoint, PointF(DesRect.Width * 0.5, DesRect.Height * 0.5), Arc1.StartAngle, Arc1.EndAngle, 1, Arc1.Stroke);
                  end;
               3: // Path
                  begin
                       // Path 渐层涂色会闪退, 原生绘图正常
                       Path1.Data.FitToRect(DesRect);
                       Canvas.FillPath(Path1.Data, 1, Path1.Fill);
                       Canvas.DrawPath(Path1.Data, 1, Path1.Stroke);
                  end;
          end;
     end;

     {$IFDEF UseNativeDraw}end);{$ENDIF} // 原生绘图 by Aone, 结束后会显示这个 Bitmap
end;

procedure TForm1.LineThicknessSpinBoxChange(Sender: TObject);
begin
     Repaint;
end;

function TForm1.NowShape: TShape;
begin
     case ShapeListBox.ItemIndex of
          0: Result := Rectangle1;
          1: Result := Ellipse1;
          2: Result := Arc1;
          3: Result := Path1;
     else    Result := Rectangle1;
     end;
end;

procedure TForm1.ShapeListBoxChange(Sender: TObject);
var Shape1: TShape;
begin
     Shape1 := NowShape;

     if Shape1 is TRectangle then
     begin
          TRectangle(Shape1).XRadius := RadiusSpinBox.Value;
          TRectangle(Shape1).YRadius := RadiusSpinBox.Value;
     end;

     if Shape1 is TArc then
     begin
          Arc1.StartAngle := ArcStartAngleTrackBar.Value;
          Arc1.EndAngle   := ArcEndAngleTrackBar.Value;
     end;

     Shape1.Stroke.Cap           := TStrokeCap(CapListBox.ItemIndex);
     Shape1.Stroke.Dash          := TStrokeDash(DashListBox.ItemIndex);
     Shape1.Stroke.Thickness     := ThicknessSpinBox.Value;

     Shape1.Stroke.Color           := StrokeColor.Color;
     Shape1.Stroke.Gradient.Color  := StrokeColor.Color;
     Shape1.Stroke.Gradient.Color1 := StrokeColor1.Color;

     Shape1.Fill.Color           := FillColor.Color;
     Shape1.Fill.Gradient.Color  := FillColor.Color;
     Shape1.Fill.Gradient.Color1 := FillColor1.Color;

     if FillGradientChk.IsChecked then
          Shape1.Fill.Kind := TBrushKind.Gradient
     else Shape1.Fill.Kind := TBrushKind.Solid;

     if StrokeGradientChk.IsChecked then
          Shape1.Stroke.Kind := TBrushKind.Gradient
     else Shape1.Stroke.Kind := TBrushKind.Solid;

     Rectangle1.Visible := (ShapeListBox.ItemIndex = 0);
     Ellipse1.Visible   := (ShapeListBox.ItemIndex = 1);
     Arc1.Visible       := (ShapeListBox.ItemIndex = 2);
     Path1.Visible      := (ShapeListBox.ItemIndex = 3);

     RadiusSpinBox.Visible := Rectangle1.Visible;
     ArcLayout.Visible     := Arc1.Visible;

     Repaint;
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
     BackRectangle.Visible := (TabControl1.ActiveTab = ShapeTab);
end;

procedure TForm1.ThicknessSpinBoxChange(Sender: TObject);
var Shape1: TShape;
begin
     Shape1 := NowShape;

     if Sender = ThicknessSpinBox then
     begin
          Shape1.Stroke.Thickness := ThicknessSpinBox.Value;
     end
     else
     if (Sender = RadiusSpinBox) and (Shape1 is TRectangle) then
     begin
          TRectangle(Shape1).XRadius := RadiusSpinBox.Value;
          TRectangle(Shape1).YRadius := RadiusSpinBox.Value;
     end
     else
     if (Sender = ArcStartAngleTrackBar) and (Shape1 is TArc) then
     begin
          Arc1.StartAngle := ArcStartAngleTrackBar.Value;
     end
     else
     if (Sender = ArcEndAngleTrackBar) and (Shape1 is TArc) then
     begin
          Arc1.EndAngle := ArcEndAngleTrackBar.Value;
     end
     else
     if Sender = StrokeColor then
     begin
          Shape1.Stroke.Color := StrokeColor.Color;
     end
     else
     if Sender = StrokeColor1 then
     begin
          StrokeGradientChk.IsChecked := True;
          Shape1.Stroke.Kind            := TBrushKind.Gradient;
          Shape1.Stroke.Gradient.Color  := StrokeColor.Color;
          Shape1.Stroke.Gradient.Color1 := StrokeColor1.Color;
     end
     else
     if Sender = StrokeGradientChk then
     begin
          if StrokeGradientChk.IsChecked then
               Shape1.Stroke.Kind := TBrushKind.Gradient
          else Shape1.Stroke.Kind := TBrushKind.Solid;
     end
     else
     if Sender = FillColor then
     begin
          Shape1.Fill.Color := FillColor.Color;
     end
     else
     if Sender = FillColor1 then
     begin
          FillGradientChk.IsChecked       := True;
          Shape1.Fill.Kind            := TBrushKind.Gradient;
          Shape1.Fill.Gradient.Color  := FillColor.Color;
          Shape1.Fill.Gradient.Color1 := FillColor1.Color;
     end
     else
     if Sender = FillGradientChk then
     begin
          if FillGradientChk.IsChecked then
               Shape1.Fill.Kind := TBrushKind.Gradient
          else Shape1.Fill.Kind := TBrushKind.Solid;
     end
     else
     if Sender = CapListBox then
     begin
          Shape1.Stroke.Cap := TStrokeCap(CapListBox.ItemIndex);
     end
     else
     if Sender = DashListBox then
     begin
          Shape1.Stroke.Dash := TStrokeDash(DashListBox.ItemIndex);
     end;

     Repaint;
end;

procedure TForm1.CapListBoxChange(Sender: TObject);
begin
     ThicknessSpinBoxChange(CapListBox);
end;

procedure TForm1.DashListBoxChange(Sender: TObject);
begin
     ThicknessSpinBoxChange(DashListBox);
end;

end.
