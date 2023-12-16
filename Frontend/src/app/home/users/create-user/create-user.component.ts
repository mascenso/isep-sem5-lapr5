import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import { RobotTypeService } from 'src/app/services/robot-type.service';
import { RobotTypeResponseDTO } from 'src/dto/robotTypeDTO';
import UserRoles from '../../../../../../Gestao_Informacao/src/enums/userRole';

@Component({
  selector: 'app-create-user',
  templateUrl: './create-user.component.html',
  styleUrls: ['./create-user.component.css']
})
export class CreateUserComponent {
  createUserForm = new FormGroup({
    email: new FormControl(''),
    password: new FormControl('', [Validators.required, Validators.pattern("/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{10,}$/")]),
    //password: new FormControl(''),
    userRole: new FormControl('')
  });

  createdRobotType: RobotTypeResponseDTO | undefined;

  UserRoles = Object.values(UserRoles);

  constructor(private robotTypeService: RobotTypeService,
    private _snackBar: MatSnackBar) {
}

public onSubmit() {
  /*
  this.robotTypeService.createRobotType(this.createUserForm.value as CreateRobotTypeRequestDTO).subscribe(
    response => {
        this.createUserForm = response;
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
  */
}

}
