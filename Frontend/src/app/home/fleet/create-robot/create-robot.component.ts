import { Component } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {MatSnackBar} from "@angular/material/snack-bar";
import { Subscription } from "rxjs";
import { RobotService } from "../../../services/robot.service";
import { RobotDTO, RobotResponseDTO } from "../../../../dto/robotDTO";
import { RobotTypeResponseDTO } from "../../../../dto/robotTypeDTO";
import { RobotTypeService } from 'src/app/services/robot-type.service';

@Component({
  selector: 'app-create-robot',
  templateUrl: './create-robot.component.html',
  styleUrls: ['./create-robot.component.css']
})
export class CreateRobotComponent {
  robotForm = new FormGroup({
    nickName: new FormControl('', [Validators.required, Validators.pattern("^[A-Z][0-9]{9}$")]),
    robotType: new FormControl(''),
    serialNumber: new FormControl('', [Validators.required, Validators.pattern("^[0-9]*$")]),
    description: new FormControl(''),
    inhibited: new FormControl(false, Validators.required)
  });

  robotTypeList: RobotTypeResponseDTO[] = [];
  robotTypeSelectionControl = new FormControl();
  robotTypeServiceSubscription$ = new Subscription();

  createdRobot: RobotDTO | undefined;

  constructor(private robotService: RobotService,
              private robotTypeService: RobotTypeService,
              private _snackBar: MatSnackBar) {
  }

  ngOnInit(): void {
    this.robotTypeServiceSubscription$ = this.robotTypeService.getAllRobotTypes().subscribe(
      response => {
        this.robotTypeList = response;
        console.log(this.robotTypeList = response);
      },

    )
  }

  public onSubmit() {
    console.log(this.robotTypeSelectionControl.value);
    this.robotForm.patchValue({
      robotType: String(this.robotTypeSelectionControl.value)
    });
    console.log("teste");
    console.log(this.robotForm.value);

    this.robotService.createRobot(this.robotForm.value as RobotDTO, true).subscribe(
      response => {
          this.createdRobot = response;
          this._snackBar.open("Robot created!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
      },
      error => {
          console.log('Error creating robot: ', error);
          this._snackBar.open(error.message, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
      }
    );
  }

  onRobotTypeSelectionChange($event: any) {
    console.log($event);
    if ($event) {
      console.log(this.robotTypeSelectionControl.value);
      // Update the form's robotType control with the selected value
      this.robotForm.patchValue({
        robotType: this.robotTypeSelectionControl.value
      });
    }
  }

}
