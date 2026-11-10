VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxSink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Src As VxSrc
Public Tag As String
Public Veto As Boolean
Public Boom As Long
Public Clear As Boolean

Private Sub Class_Terminate()
    XLog.Lg "sink term " & Tag
End Sub

Private Sub Src_Ping(ByVal n As Long)
    XLog.Lg Tag & " ping " & n
    If Boom <> 0 Then Err.Raise Boom
    If Clear Then Set Src = Nothing
End Sub

Private Sub Src_Veto(Cancel As Boolean)
    XLog.Lg Tag & " veto " & Cancel
    If Veto Then Cancel = True
End Sub

Private Sub Src_Modify(s As String)
    s = s & Tag
End Sub

Private Sub Src_Many(ByVal a As Long, ByVal b As String, ByVal c As Double)
    XLog.Lg Tag & " many " & a & b & c
End Sub
