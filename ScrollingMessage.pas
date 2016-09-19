unit ScrollingMessage;

interface

uses
  SysUtils, WinTypes, WinProcs, Classes, Forms, Controls, Graphics,
  Messages, ExtCtrls, Dialogs;

const
  ScrollPixels = 2;
  TimerInterval = 50;

type
  TJustification = (tjCenter, tjLeft, tjRight);

  EMarqueeError = class(Exception);

  TScrollingMessage = class(TCustomPanel)
  private
    MemBitmap: TBitmap;
    InsideRect: TRect;
    FItems: TStringList;
    FJust: TJustification;
    FScrollDown: Boolean;
    LineHi : integer;
    CurrLine : integer;
    VRect: TRect;
    FTimer: TTimer;
    FActive: Boolean;
    FOnDone: TNotifyEvent;
  protected
    procedure SetItems(Value: TStringList); virtual;
    procedure FTimerOnTimer(Sender: TObject); virtual;
    procedure MakeRects; virtual;
    procedure PaintLine(R: TRect; LineNum: integer); virtual;
    procedure SetLineHeight; virtual;
    procedure SetStartLine; virtual;
    procedure IncLine; virtual;
  protected
    procedure Paint; override;
    procedure FillBitmap; virtual;
  public
    property Active: Boolean read FActive;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
  published
    property ScrollDown: Boolean read FScrollDown write FScrollDown;
    property Justify: TJustification read FJust write FJust default tjCenter;
    property Items: TStringList read FItems write SetItems;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Font;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

procedure TMarquee.FTimerOnTimer(Sender: TObject);
{ This method is executed in respose to a timer event }
begin
  IncLine;
  InvalidateRect(Handle, @InsideRect, False);   { only repaint within borders }
end;

procedure TMarquee.IncLine;
{ this method is called to increment a line }
begin
  if not FScrollDown then         { if Marquee is scrolling upward }
  begin
    { Check to see if marquee has scrolled to end yet }
    if FItems.Count * LineHi + ClientRect.Bottom - ScrollPixels  >= CurrLine then
      inc(CurrLine, ScrollPixels) { not at end, so increment current line }
    else Deactivate;
  end
  else begin                      { if Marquee is scrolling downward }
    { Check to see if marquee has scrolled to end yet }
    if CurrLine >= ScrollPixels then
      dec(CurrLine, ScrollPixels) { not at end, so decrement current line }
    else Deactivate;
  end;
end;

constructor TMarquee.Create(AOwner: TComponent);
{ constructor for TMarquee class }

  procedure DoTimer;
  { procedure sets up TMarquee's timer }
  begin
    FTimer := TTimer.Create(Self);
    with FTimer do begin
      Enabled := False;
      Interval := TimerInterval;
      OnTimer := FTimerOnTimer;
    end;
  end;

begin
  inherited Create(AOwner);
  FItems := TStringList.Create;  { instanciate string list }
  DoTimer;                       { set up timer }

  { set instance variable default values }
  Width := 100;
  Height := 75;
  FActive := False;
  FScrollDown := False;
  FJust := tjCenter;
  BevelWidth := 3;
end;

destructor TMarquee.Destroy;
{ destructor for TMarquee class }
begin
  Deactivate;
  FTimer.Free;                   { free allocated classes }
  FItems.Free;
  inherited Destroy;
end;

procedure TMarquee.SetItems(Value: TStringList);
begin
  if FItems <> Value then
    FItems.Assign(Value);
end;

procedure TMarquee.SetLineHeight;
{ this virtual method sets the LineHi instance variable }
var
  Metrics : TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, Metrics);  { get metric info for font }
  LineHi := Metrics.tmHeight + Metrics.tmInternalLeading; { adjust line height }
end;

procedure TMarquee.SetStartLine;
{ this virtual method initializes the CurrLine instance variable }
begin
  if not FScrollDown then                  { initialize current line to... }
    CurrLine := 0                          { top if scrolling up, or }
  else
    CurrLine := VRect.Bottom - Height;     { bottom if scrolling down }
end;

procedure TMarquee.PaintLine(R: TRect; LineNum: integer);
{ this method is called to paint each line of text onto MemBitmap }
const
  Flags: array[TJustification] of Longint = (dt_Center, dt_Left, dt_Right);
var
  p: array[0..255]of char;
begin
  StrPCopy(p, FItems.Strings[LineNum]);
  if DrawText(MemBitmap.Canvas.Handle, p, StrLen(p), R,
           Flags[FJust] or dt_SingleLine or dt_Top) <= 0 then
    raise EMarqueeError.Create('Failed to render text');
end;

procedure TMarquee.MakeRects;
{ procedure sets up VRect and InsideRect TRects }
begin
  { VRect rectangle represents entire memory bitmap }
  with VRect do begin
    Top := 0;
    Left := 0;
    Right := Width;
    Bottom := LineHi * FItems.Count + Height * 2;
  end;
  { InsideRect rectangle represents interior of beveled border }
  with InsideRect do begin
    Top := BevelWidth;
    Left := BevelWidth;
    Right := Width - (2 * BevelWidth);
    Bottom := Height - (2 * BevelWidth);
  end;
end;

procedure TMarquee.FillBitmap;
var
  y, i : integer;
  Rect: TRect;
begin
  SetLineHeight;                         { set height of each line }
  MakeRects;                             { make rectangles }
  with Rect do begin
    Left := InsideRect.Left;
    Bottom := VRect.Bottom ;
    Right := InsideRect.Right;
  end;
  SetStartLine;
  MemBitmap.Width := Width;
  with MemBitmap do begin
    Height := VRect.Bottom;
    with Canvas do begin
      Font := Self.Font;
      Brush.Color := Color;
      FillRect(VRect);
      Brush.Style := bsClear;
    end;
  end;
  y := Height;
  i := 0;
  repeat
    Rect.Top := y;
    PaintLine(Rect, i);
    inc(y, LineHi);  { increment y by the height (in pixels) of a line }
    inc(i);
  until i >= FItems.Count;               { repeat for all lines }
end;

procedure TMarquee.Activate;
{ this method is called to activate the marquee }
begin
  if (not FActive) and (FItems.Count > 0) then begin
    FActive := True;                       { set active flag }
    MemBitmap := TBitmap.Create;
    FillBitmap;                            { Paint Image on bitmap }
    FTimer.Enabled := True;                { start timer }
  end;
end;

procedure TMarquee.Deactivate;
begin
  if FActive then begin
    FTimer.Enabled := False;                 { disable timer, }
    if Assigned(FOnDone) then FOnDone(Self); { fire OnDone event, }
    FActive := False;                        { set FActive to False }
    MemBitmap.Free;                          { free memory bitmap }
    Invalidate;                              { clear control window }
  end;
end;

procedure TMarquee.Paint;
{ this virtual method is called in response to a Windows paint message }
begin
  if FActive then
    BitBlt(Canvas.Handle, 0, 0, InsideRect.Right, InsideRect.Bottom,
           MemBitmap.Canvas.Handle, 0, CurrLine, srcCopy)
  else
    inherited Paint;
end;

end.
