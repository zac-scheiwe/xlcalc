Attribute VB_Name = "R9Types"
Option Explicit

Public R9Log As String

Public Type R9Pt
    X As Long
    Y As Double
    S As String
End Type

Public Type R9Rec
    Id As Integer
    Nm As String * 6
    Tags(1 To 3) As Byte
    Pt As R9Pt
    Cur As Currency
End Type

Public Type R9Fix
    A As Byte
    B As String * 4
    C As Long
End Type

Public Type R9Raw
    Raw As String * 9
End Type

Public Type R9Bytes
    Bt(0 To 8) As Byte
End Type

Public GPt As R9Pt
Public GRecs(1 To 3) As R9Rec

Public Function MkPt(ByVal x As Long, ByVal y As Double, ByVal s As String) As R9Pt
    Dim t As R9Pt
    t.X = x
    t.Y = y
    t.S = s
    MkPt = t
End Function

Public Sub BumpPt(ByRef p As R9Pt)
    p.X = p.X + 1
    p.Y = p.Y * 2
    p.S = p.S & "!"
End Sub

Public Function PtStr(ByRef p As R9Pt) As String
    PtStr = p.X & "/" & p.Y & "/" & p.S
End Function

Public Sub SetRec(ByRef r As R9Rec, ByVal id As Integer, ByVal nm As String)
    r.Id = id
    r.Nm = nm
    r.Tags(1) = id Mod 256
    r.Tags(3) = 9
    r.Pt.X = id * 2
    r.Pt.S = nm
    r.Cur = id / 4
End Sub

Public Function RecStr(ByRef r As R9Rec) As String
    RecStr = r.Id & "|" & r.Nm & "|" & r.Tags(1) & r.Tags(2) & r.Tags(3) & "|" & r.Pt.X & r.Pt.S & "|" & r.Cur
End Function

Public Function RecArrStr(ByRef a() As R9Rec) As String
    Dim i As Long, s As String
    For i = LBound(a) To UBound(a)
        s = s & RecStr(a(i)) & ";"
    Next i
    RecArrStr = s
End Function

Public Function PtOf(ByVal n As Long) As R9Pt
    PtOf = MkPt(n, n / 2, "p" & n)
End Function
