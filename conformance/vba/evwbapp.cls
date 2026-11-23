VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "EvWbApp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application

Private Sub App_WorkbookActivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvWB.Add "A:WbA" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub

Private Sub App_WorkbookDeactivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvWB.Add "A:WbD" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub

Private Sub App_WindowActivate(ByVal Wb As Workbook, ByVal Wn As Window)
    On Error GoTo EH
    EvWB.Add "A:WA" & ":" & Wn.Caption
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub

Private Sub App_WindowDeactivate(ByVal Wb As Workbook, ByVal Wn As Window)
    On Error GoTo EH
    EvWB.Add "A:WD" & ":" & Wn.Caption
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub

Private Sub App_WorkbookBeforeClose(ByVal Wb As Workbook, Cancel As Boolean)
    On Error GoTo EH
    EvWB.Add "A:BC" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub

Private Sub App_NewWorkbook(ByVal Wb As Workbook)
    On Error GoTo EH
    EvWB.Add "A:New" & ":" & Wb.Name
    Exit Sub
EH:
    EvWB.Add "A error"
End Sub
