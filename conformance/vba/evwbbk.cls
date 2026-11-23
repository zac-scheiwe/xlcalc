VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "EvWbBk"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents Wb As Workbook
Public Tag As String

Private Sub Wb_Activate()
    On Error GoTo EH
    EvWB.Add Tag & ":WbA" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub

Private Sub Wb_Deactivate()
    On Error GoTo EH
    EvWB.Add Tag & ":WbD" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub

Private Sub Wb_WindowActivate(ByVal Wn As Window)
    On Error GoTo EH
    EvWB.Add Tag & ":WA" & ":" & Wn.Caption
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub

Private Sub Wb_WindowDeactivate(ByVal Wn As Window)
    On Error GoTo EH
    EvWB.Add Tag & ":WD" & ":" & Wn.Caption
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub

Private Sub Wb_BeforeClose(Cancel As Boolean)
    On Error GoTo EH
    EvWB.Add Tag & ":BC" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub

Private Sub Wb_NewWindow(ByVal Wn As Window)
    On Error GoTo EH
    EvWB.Add Tag & ":NW" & ":" & Wn.Caption
    Exit Sub
EH:
    EvWB.Add "B error"
End Sub
