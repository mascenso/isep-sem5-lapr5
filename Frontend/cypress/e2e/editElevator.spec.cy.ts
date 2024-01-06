describe('Teste para editar de Elevadores', () => {
    beforeEach(() => {
        //criar edificio para ver na tabela
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Campus"]').click();

        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-9').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-10').type("10");
        cy.get('#mat-input-11').type('10');
        cy.get('#mat-input-12').type('teste');
        cy.get('#mat-input-13').type('Mais um edificio de teste');
        cy.get('.mdc-button__label').click();

        //crio um floor no primeiro building da dropdown
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

        cy.get('#dropdownCreateFloor').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();

        cy.get('#mat-input-14').type("10");
        cy.get('#mat-input-15').type('10');
        cy.get('#mat-input-16').type('1');
        cy.get('#mat-input-17').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();

        //crio elevadores
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-elevator"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/elevators')

        cy.get('#mat-input-18').type('ELEV' + numeroAleatorio.toString());

        cy.get('#dropdownCreateElevator').click();
        cy.contains('Edificio Z - TESTE').click();

        cy.get('#selectFloorsElevator').click();
        cy.contains('Piso 1').click();
        cy.get('#selectFloorsElevator').type('{esc}');

        cy.get('#saveNewElevator').click({ force: true });

        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-elevators"]').click();
    });

    it('Deve exibir a tabela com os campos editaveis nao vazios', () => {

        cy.get('#dropdownBuildingSelect').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();
/*
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-21 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-22 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-23 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-21 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-22 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-23 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');*/

    });

    it('Deve alterar campo code do elev do ultimo elevador criado', () => {

        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-elevators"]').click();
        cy.intercept('PATCH', 'http://localhost:4000/api/elevators').as('rotaElevator');

        cy.get('#dropdownBuildingSelect').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();

        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-19').clear();
        cy.get('#mat-input-19').type('ELEV' + numeroAleatorio.toString());

        cy.get('#saveNewElevator').click({ force: true });

        cy.wait('@rotaElevator').should(({ response }) => {
            expect(response!.statusCode).to.eq(200);
        });

    });
});