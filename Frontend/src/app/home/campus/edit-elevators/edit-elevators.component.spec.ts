import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditElevatorsComponent } from './edit-elevators.component';

describe('EditElevatorsComponent', () => {
  let component: EditElevatorsComponent;
  let fixture: ComponentFixture<EditElevatorsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [EditElevatorsComponent]
    });
    fixture = TestBed.createComponent(EditElevatorsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
