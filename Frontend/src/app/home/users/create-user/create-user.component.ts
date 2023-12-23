import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import { UserService } from '../../../services/user.service';
import { UserResponseDTO, CreateUserRequestDTO } from '../../../../dto/userDTO';
import UserRoles from '../../../../../../Gestao_Informacao/src/enums/userRole';

@Component({
  selector: 'app-create-user',
  templateUrl: './create-user.component.html',
  styleUrls: ['./create-user.component.css']
})
export class CreateUserComponent {
  createUserForm = new FormGroup({
    email: new FormControl(''),
    password: new FormControl('', [Validators.required]),
    firstName: new FormControl(''),
    lastName: new FormControl(''),
    userRole: new FormControl(''),
    active: new FormControl(false)
  });

  createdUser: UserResponseDTO | undefined;

  UserRoles = Object.values(UserRoles);

  constructor(private userService: UserService,
    private _snackBar: MatSnackBar) {
}

public onSubmit() {
  this.userService.registerUser(this.createUserForm.value as CreateUserRequestDTO, true).subscribe(
    response => {
        this.createdUser = response;
        this._snackBar.open("User created!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
    },
    error => {
        console.log('Error creating User: ', error);
        this._snackBar.open(error.message, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
    }
  );

  
}

}
