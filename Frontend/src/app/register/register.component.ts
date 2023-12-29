import { Component, OnInit } from '@angular/core';
import { PrivacyPolicyComponent } from '../../app/privacy-policy/privacy-policy.component';
import { MatDialog } from '@angular/material/dialog';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {UserService} from "../services/user.service";
import {CreateUserRequestDTO} from "../../dto/userDTO";
import {MatSnackBar} from "@angular/material/snack-bar";
import {Router} from "@angular/router";

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.css']
})
export class RegisterComponent implements OnInit {


  registerForm = new FormGroup({
    email: new FormControl('', [Validators.required, Validators.email]),
    password: new FormControl('', [Validators.required]),
    firstName: new FormControl('', [Validators.required]),
    lastName: new FormControl('', [Validators.required]),
    termsAndConditions: new FormControl(false, [Validators.requiredTrue]),
    taxPayerNumber: new FormControl('', [Validators.required]),
    mechanographicNumber: new FormControl('', [Validators.required]),
    phoneNumber: new FormControl('', [Validators.required]),
  });

  hidePassword = true;

  constructor(private dialog: MatDialog,
              private userService: UserService,
              private _snackBar: MatSnackBar,
              private router: Router) { }
  ngOnInit() {}

  openPrivacyPolicyDialog(): void {
    this.dialog.open(PrivacyPolicyComponent, {
      width: '100%', // Set width as needed
      panelClass: 'terms-modalbox'
    });
  }

  getErrorMessage() {
    if (this.registerForm.controls.email.hasError('required')) {
      return 'You must enter a value';
    }

    return this.registerForm.controls.email.hasError('email') ? 'Not a valid email' : '';
  }

  onSubmit() {
    if (!this.registerForm.invalid) {
      const createUserRequestDto: CreateUserRequestDTO = {
        email: this.registerForm.controls.email.value!,
        password: this.registerForm.controls.password.value!,
        firstName: this.registerForm.controls.firstName.value!,
        lastName: this.registerForm.controls.lastName.value!,
        role: "USER",
        active: false,
        taxPayerNumber: this.registerForm.controls.taxPayerNumber.value!,
        mechanographicNumber: this.registerForm.controls.mechanographicNumber.value!,
        phoneNumber: this.registerForm.controls.phoneNumber.value!,
      }
      this.userService.registerUser(createUserRequestDto, true)
          .subscribe(
              (response) => {
                this._snackBar.open("User created successfully, once it is approved you can login into your account.", "close", {
                  duration: 10000,
                  panelClass: ['snackbar-success']
                });
                this.router.navigate(['login'])
              },
              (error) => {
                this._snackBar.open(error.error.message, "close", {
                  duration: 5000,
                  panelClass: ['snackbar-error']
                })
              }
          )
    }
  }

}
