unit ONE.Objects;

{$i ..\NativeDraw.inc}

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Classes, System.Rtti, System.Messaging,
  System.Math,

  FMX.Graphics.Native,

  FMX.Types, FMX.Controls, FMX.TextLayout, FMX.Objects,
  FMX.Platform, FMX.Graphics, FMX.MultiResBitmap, FMX.ActnList;

type

  TOneArc = class(TEllipse)
  private
    FStartAngle: Single;
    FEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    procedure SetStartAngle(const Value: Single);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property EndAngle: Single read FEndAngle write SetEndAngle;
  end;

implementation

function GetDrawingShapeRectAndSetThickness(const AShape: TShape; const Fit: Boolean; var FillShape, DrawShape: Boolean;
  var StrokeThicknessRestoreValue: Single): TRectF;
const
  MinRectAreaSize = 0.01;
begin
  FillShape := (AShape.Fill <> nil) and (AShape.Fill.Kind <> TBrushKind.None);
  DrawShape := (AShape.Stroke <> nil) and (AShape.Stroke.Kind <> TBrushKind.None);

  if Fit then
    Result := TRectF.Create(0, 0, 1, 1).FitInto(AShape.LocalRect)
  else
    Result := AShape.LocalRect;

  if DrawShape then
  begin
    if Result.Width < AShape.Stroke.Thickness then
    begin
      StrokeThicknessRestoreValue := AShape.Stroke.Thickness;
      FillShape := False;
      AShape.Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Left := (Result.Right + Result.Left) * 0.5;
      Result.Right := Result.Left + MinRectAreaSize;
    end
    else
      Result.Inflate(-AShape.Stroke.Thickness * 0.5, 0);

    if Result.Height < AShape.Stroke.Thickness then
    begin
      if StrokeThicknessRestoreValue < 0.0 then
        StrokeThicknessRestoreValue := AShape.Stroke.Thickness;
      FillShape := False;
      AShape.Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Top := (Result.Bottom + Result.Top) * 0.5;
      Result.Bottom := Result.Top + MinRectAreaSize;
    end
    else
      Result.Inflate(0, -AShape.Stroke.Thickness * 0.5);
  end;
end;

constructor TOneArc.Create(AOwner: TComponent);
begin
  inherited;
  Fill.Kind := TBrushKind.None;
  Fill.DefaultKind := TBrushKind.None;
  FStartAngle := 0;
  FEndAngle := -90;
end;

procedure TOneArc.Paint;
var
  LShapeRect: TRectF;
  MidPoint: TPointF;
  StrokeThicknessRestoreValue: Single;
  FillShape, DrawShape: Boolean;
  SrcRect: TRectF;
begin
  SrcRect := LocalRect;
  {$IFDEF UseNativeDraw}Canvas.NativeDraw(SrcRect{注意:请勿在昵名函数里改变它}, procedure var i: Integer; begin {$ENDIF} // 原生绘图 by Aone, 昵名函数里加入绘图方法, 内部会先画到 Bitmap

  StrokeThicknessRestoreValue := Stroke.Thickness;
  try
    LShapeRect := GetDrawingShapeRectAndSetThickness(Self, False, FillShape, DrawShape, StrokeThicknessRestoreValue);
    MidPoint := LShapeRect.CenterPoint;

    if FillShape then
      Canvas.FillArc(MidPoint, TPointF.Create(LShapeRect.Width * 0.5, LShapeRect.Height * 0.5), FStartAngle, FEndAngle,
        AbsoluteOpacity, Fill);
    if DrawShape then
      Canvas.DrawArc(MidPoint, TPointF.Create(LShapeRect.Width * 0.5, LShapeRect.Height * 0.5), FStartAngle, FEndAngle,
        AbsoluteOpacity, Stroke);
  finally
    if StrokeThicknessRestoreValue <> Stroke.Thickness then
      Stroke.Thickness := StrokeThicknessRestoreValue;
  end;

  {$IFDEF UseNativeDraw}end);{$ENDIF} // 原生绘图 by Aone, 结束后会显示这个 Bitmap
end;

procedure TOneArc.SetEndAngle(const Value: Single);
begin
  if FEndAngle <> Value then
  begin
    FEndAngle := Value;
    Repaint;
  end;
end;

procedure TOneArc.SetStartAngle(const Value: Single);
begin
  if FStartAngle <> Value then
  begin
    FStartAngle := Value;
    Repaint;
  end;
end;

initialization
  RegisterFmxClasses([TOneArc]);
finalization

end.
