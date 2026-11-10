VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxLife"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Tag As String
Public Child As VxLife
Public Other As VxLife

Private Sub Class_Initialize()
    XLog.Lg "init"
End Sub

Private Sub Class_Terminate()
    XLog.Lg "term " & Tag
End Sub

Public Function Hello() As String
    Hello = "hi " & Tag
End Function
