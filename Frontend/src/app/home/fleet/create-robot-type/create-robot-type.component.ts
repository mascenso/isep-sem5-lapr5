import { Component } from '@angular/core';
import {FormArray, FormControl, FormGroup, Validators} from "@angular/forms";
import {RobotTypeResponseDto, RobotTypeService, CreateRobotTypeRequestDto} from "../../../services/robot-type.service";
import {MatSnackBar, MatSnackBarConfig} from "@angular/material/snack-bar";
import TaskType from "../../../../../../Gestao_Informacao/src/enums/taskType";

@Component({
  selector: 'app-create-robot-type',
  templateUrl: './create-robot-type.component.html',
  styleUrls: ['./create-robot-type.component.css']
})

export class CreateRobotTypeComponent {
  
  
  robotTypeForm = new FormGroup({
    designacao: new FormControl(''),
    tipoTarefas: new FormControl(''),
  });

  createdRobotType: RobotTypeResponseDto | undefined;

  taskTypes = Object.values(TaskType);

  constructor(private robotTypeService: RobotTypeService,
    private _snackBar: MatSnackBar) {
}

public onSubmit() {
  this.robotTypeService.createRobotType(this.robotTypeForm.value as CreateRobotTypeRequestDto).subscribe(
    response => {
        this.createdRobotType = response;
        this._snackBar.open("Robot Type created!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
    },
    error => {
        console.log('Error creating Robot Type: ', error);
        this._snackBar.open(error.message, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
    }
  );
}

}
