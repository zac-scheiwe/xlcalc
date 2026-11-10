VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R8Sink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Src As R8Box
Public Id As String
Public Veto As Boolean
Public Boom As Long
Public Detach As Boolean

Private Sub Class_Terminate()
    R8Log = R8Log & "t" & Id
End Sub

Private Sub Src_Changed(ByVal n As Long, Cancel As Boolean)
    R8Log = R8Log & "<" & Id & "ch" & n & ">"
    If Boom <> 0 Then Err.Raise Boom
    If Detach Then Set Src = Nothing
    If Veto Then Cancel = True
End Sub

Private Sub Src_Plain()
    R8Log = R8Log & "<" & Id & "pl>"
    If Boom <> 0 Then Err.Raise Boom
    If Detach Then Set Src = Nothing
End Sub

Private Sub Src_Msg(s As String)
    R8Log = R8Log & "<" & Id & "m" & s & ">"
    s = s & Id
End Sub
