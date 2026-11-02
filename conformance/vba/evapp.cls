VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "EvApp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application

Private Sub App_NewWorkbook(ByVal Wb As Workbook)
    On Error GoTo EH
    EvLog.Enter "App", "NewWorkbook"
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.NewWorkbook", Err.Number
End Sub

Private Sub App_WorkbookOpen(ByVal Wb As Workbook)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookOpen", Nothing, Nothing, Wb.Name
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookOpen", Err.Number
End Sub

Private Sub App_WorkbookActivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookActivate"
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookActivate", Err.Number
End Sub

Private Sub App_WorkbookDeactivate(ByVal Wb As Workbook)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookDeactivate"
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookDeactivate", Err.Number
End Sub

Private Sub App_WorkbookBeforeClose(ByVal Wb As Workbook, Cancel As Boolean)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookBeforeClose", Nothing, Nothing, "Cancel=" & Cancel
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookBeforeClose", Err.Number
End Sub

Private Sub App_WorkbookBeforeSave(ByVal Wb As Workbook, ByVal SaveAsUI As Boolean, Cancel As Boolean)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookBeforeSave", Nothing, Nothing, "SaveAsUI=" & SaveAsUI & " Cancel=" & Cancel
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookBeforeSave", Err.Number
End Sub

Private Sub App_WorkbookAfterSave(ByVal Wb As Workbook, ByVal Success As Boolean)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookAfterSave", Nothing, Nothing, "Success=" & Success
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookAfterSave", Err.Number
End Sub

Private Sub App_WorkbookNewSheet(ByVal Wb As Workbook, ByVal Sh As Object)
    On Error GoTo EH
    EvLog.Enter "App", "WorkbookNewSheet", Sh
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.WorkbookNewSheet", Err.Number
End Sub

Private Sub App_SheetActivate(ByVal Sh As Object)
    On Error GoTo EH
    EvLog.Enter "App", "SheetActivate", Sh
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetActivate", Err.Number
End Sub

Private Sub App_SheetDeactivate(ByVal Sh As Object)
    On Error GoTo EH
    EvLog.Enter "App", "SheetDeactivate", Sh
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetDeactivate", Err.Number
End Sub

Private Sub App_SheetBeforeDelete(ByVal Sh As Object)
    On Error GoTo EH
    EvLog.Enter "App", "SheetBeforeDelete", Sh
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetBeforeDelete", Err.Number
End Sub

Private Sub App_SheetCalculate(ByVal Sh As Object)
    On Error GoTo EH
    EvLog.Enter "App", "SheetCalculate", Sh
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetCalculate", Err.Number
End Sub

Private Sub App_SheetChange(ByVal Sh As Object, ByVal Target As Range)
    On Error GoTo EH
    EvLog.Enter "App", "SheetChange", Sh, Target
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetChange", Err.Number
End Sub

Private Sub App_SheetSelectionChange(ByVal Sh As Object, ByVal Target As Range)
    On Error GoTo EH
    EvLog.Enter "App", "SheetSelectionChange", Sh, Target
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.SheetSelectionChange", Err.Number
End Sub

Private Sub App_AfterCalculate()
    On Error GoTo EH
    EvLog.Enter "App", "AfterCalculate"
    EvLog.Leave
    Exit Sub
EH:
    EvLog.Fail "App.AfterCalculate", Err.Number
End Sub
