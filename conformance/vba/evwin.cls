VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "EvWinApp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application

Private Sub App_WorkbookActivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvW.Add "A:" & Wb.Name
    Exit Sub
EH:
    EvW.Add "A error"
End Sub

Private Sub App_WorkbookDeactivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvW.Add "D:" & Wb.Name
    Exit Sub
EH:
    EvW.Add "D error"
End Sub

Private Sub App_WindowActivate(ByVal Wb As Workbook, ByVal Wn As Window)
    On Error GoTo EH
    EvW.Add "WA:" & Wn.Caption
    Exit Sub
EH:
    EvW.Add "WA error"
End Sub

Private Sub App_WindowDeactivate(ByVal Wb As Workbook, ByVal Wn As Window)
    On Error GoTo EH
    EvW.Add "WD:" & Wn.Caption
    Exit Sub
EH:
    EvW.Add "WD error"
End Sub
