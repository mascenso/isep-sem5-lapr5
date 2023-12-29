import {Component, OnDestroy, OnInit} from '@angular/core';
import {UserService} from "../../services/user.service";
import {Subscription} from "rxjs";
import {MatSnackBar} from "@angular/material/snack-bar";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {PatchUserDataRequestDTO} from "../../../dto/patchUserDataRequestDTO";
import {Router} from "@angular/router";
import {FileSaverService} from "ngx-filesaver";
import { FileSaverOptions } from 'file-saver';

@Component({
  selector: 'app-user-profile',
  templateUrl: './user-profile.component.html',
  styleUrls: ['./user-profile.component.css']
})
export class UserProfileComponent implements OnInit, OnDestroy {

    deleteAccount = false;

    editUserDataForm = new FormGroup({
        email: new FormControl(''),
        firstName: new FormControl('', [Validators.pattern("[aA-zZ ]*")]),
        lastName: new FormControl('', [Validators.pattern("[aA-zZ ]*")]),
        role: new FormControl(''),
        taxPayerNumber: new FormControl(''),
        mechanographicNumber: new FormControl(''),
        phoneNumber: new FormControl('',),
    });

    private userSubscription = new Subscription();

    options: FileSaverOptions = {
        autoBom: false,
    };

    constructor(private userService: UserService,
                private _snackBar: MatSnackBar,
                private router: Router,
                private fileSaverService: FileSaverService) {}

    ngOnInit(): void {
      this.userSubscription = this.userService.getUserData(true)
          .subscribe(
              (response) => {
                  this.editUserDataForm.patchValue(response);
              },
              (error) => {
                  this._snackBar.open("Erro a criar piso!", "close", {
                      duration: 5000,
                      panelClass: ['snackbar-error']
                  });
              }
          )
    }

    ngOnDestroy(): void {
      this.userSubscription.unsubscribe();
    }

    public onSubmit() {
      if (!this.editUserDataForm.invalid) {
        const patchUserDataRequest: PatchUserDataRequestDTO = {
          firstName: this.editUserDataForm.controls.firstName.value,
          lastName: this.editUserDataForm.controls.lastName.value
        }
        this.userService.updateUser(patchUserDataRequest)
            .subscribe(
                (response) => {
                    this.editUserDataForm.patchValue(response);
                    this._snackBar.open("Profile updated successfully!", "close", {
                        duration: 5000,
                        panelClass: ['snackbar-success']
                    });
                    },
                (error) => {
                    this._snackBar.open("Error updating profile!", "close", {
                            duration: 5000,
                            panelClass: ['snackbar-error']
                        })
                }
            );
      }
    }

    public onDelete() {
      this.userService.deleteUser(true )
          .subscribe(
              (response) => {
                  this._snackBar.open("User account deleted successfully!", "close", {
                      duration: 5000,
                      panelClass: ['snackbar-success']
                  });
                this.router.navigate(['login']);
              },
              (error) => {
                  this._snackBar.open(error.error.message, "close", {
                      duration: 5000,
                      panelClass: ['snackbar-error']
                  })
              }
          );
    }

    public onDownload() {
        const fileName = 'user-data.txt';
        const dataBlob = new Blob([JSON.stringify(this.editUserDataForm.value)], {type:'txt'});
        this.fileSaverService.save(dataBlob, fileName, undefined, this.options);
    }

}
