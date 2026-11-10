VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R9Sink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Src As R9Src
Public Id As String
Public Mode As Long

Private Sub Class_Terminate()
    R9Log = R9Log & "t" & Id
End Sub

Private Sub Src_Ev1(ByVal n As Long, res As String)
    R9Log = R9Log & "<" & Id & "e1:" & n & ">"
    res = res & Id & n
    If Mode = 1 Then Err.Raise 1000 + n
    If Mode = 2 Then
        On Error Resume Next
        Err.Raise 5
        R9Log = R9Log & "e" & Err.Number
    End If
    If Mode = 3 Then Set Src = Nothing
    If Mode = 4 Then Src.Tag = "x"
End Sub

Private Sub Src_Ev2(a As Long, b As Variant)
    R9Log = R9Log & "<" & Id & "e2:" & a & TypeName(b) & ">"
    a = a + 1
    b = b & Id
    If Mode = 1 Then Err.Raise 9
End Sub

Private Sub Src_Ev3()
    R9Log = R9Log & "<" & Id & "e3>"
    If Mode = 1 Then Err.Raise 11
End Sub
