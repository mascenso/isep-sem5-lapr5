import { Component } from '@angular/core';
import {FormControl, FormGroup } from "@angular/forms";
import TaskType from "../../../../../../Gestao_Informacao/src/enums/taskType";
import {animate, state, style, transition, trigger} from "@angular/animations";

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

  dataSource: RobotDto[] = ROBOT_DATA;
  columnsToDisplay = ['id', 'nickName', 'robotType', 'serialNumber', 'inhibited'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: RobotDto | null | undefined;

  listRobotsForm = new FormGroup({
    taskTypeControl: new FormControl(),
    designationControl: new FormControl('')
  });

  taskTypes = Object.values(TaskType);

  onSubmit(): void {
    console.log(this.listRobotsForm);
/*    this.buildingService.createBuilding(this.buildingForm.value as CreateBuildingRequestDto, true).subscribe(
      response => {
        this.createdBuilding = response;
        this._snackBar.open("Building created!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
      },
      error => {
        console.log('Error creating building: ', error);
        this._snackBar.open(error.message, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    );*/
  }

}
export interface RobotDto {
  id: string;
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}

const ROBOT_DATA = [
  {
    id: 'robot-id-1',
    nickName: 'robotronic',
    robotType: 'ix5000',
    serialNumber: 'EAN123001230',
    description: 'Um Robot muito lindo',
    inhibited: false
  },
  {
    id: 'robot-id-2',
    nickName: 'robotronic2x',
    robotType: 'ix6000',
    serialNumber: 'EAN123001230',
    description: 'Um Robot muito lindo',
    inhibited: false
  },
  {
    id: 'robot-id-3',
    nickName: 'robotronic',
    robotType: 'ix9000',
    serialNumber: 'EAN123001230',
    description: 'Um Robot ainda mais lindo',
    inhibited: true
  }
]
