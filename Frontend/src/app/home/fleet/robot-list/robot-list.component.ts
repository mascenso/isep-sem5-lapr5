import { Component } from '@angular/core';
import {FormControl, FormGroup } from "@angular/forms";
import TaskType from "../../../../../../Gestao_Informacao/src/enums/taskType";
import {animate, state, style, transition, trigger} from "@angular/animations";
import {MatSnackBar} from "@angular/material/snack-bar";
import { RobotDTO } from "../../../../dto/robotDTO";
import { RobotService } from "../../../services/robot.service";

@Component({
  selector: 'app-robot-list',
  templateUrl: './robot-list.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./robot-list.component.css']
})
export class RobotListComponent {

  dataSource: RobotDTO[] = [];
  columnsToDisplay = ['id', 'nickName', 'robotType', 'serialNumber', 'inhibited'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: RobotDTO | null | undefined;

  listRobotsForm = new FormGroup({
    taskTypeControl: new FormControl(),
    designationControl: new FormControl('')
  });

  taskTypes = Object.values(TaskType);

  constructor(private robotService: RobotService,
              private _snackBar: MatSnackBar) {}

  onSubmit(): void {
    console.log(this.listRobotsForm.controls.taskTypeControl.value);
    console.log(this.listRobotsForm.controls.designationControl.value);
    this.robotService.findRobotsByTaskTypeOrDesignation(
      this.listRobotsForm.controls.taskTypeControl.value,
      this.listRobotsForm.controls.designationControl.value, true).subscribe(
      response => {
        console.log(response);
          this.dataSource = response;
      },
      error => {
        this._snackBar.open(error.error, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    );
  }

}
