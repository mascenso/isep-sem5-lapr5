describe('Teste de criar um elevador', () => {

    beforeEach(() => {

        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click();
    
        cy.get('[routerlink="create-elevator"]').click();
    });


    it('Devem existir todos os campos.', () => {

        cy.get('#mat-input-0').should('exist');
        cy.get('#mat-select-value-1').should('exist');
        cy.get('#mat-select-value-3').should('exist');

    });
    it('Todos os campos devem estar vazios.', () => {

        cy.get('#mat-input-0').should('have.value', '');
        cy.get('#mat-select-value-1').should('have.value', '');
        cy.get('#mat-select-value-3').should('have.value', '');

    });

    it('Deve ser possivel criar elevador e receber mensagem de sucesso.', () => {
        //crio um building para garantir que tenho pelo menos um na dropdown do floor e do elevator
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-1').type('A' + numeroAleatorio.toString());
        cy.get('#mat-input-2').type('10');
        cy.get('#mat-input-3').type('50');
        cy.get('#mat-input-4').type('Edificio A');
        cy.get('#mat-input-5').type('Edificio A - Administracao');
        cy.get('.mdc-button__label').click();

        //crio um floor no primeiro building da dropdown
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

        cy.get('#dropdownCreateFloor').click().get('#mat-option-2').click();

        cy.get('#mat-input-6').type("10");
        cy.get('#mat-input-7').type('10');
        cy.get('#mat-input-8').type('4');
        cy.get('#mat-input-9').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();

        //crio elevadores
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-elevator"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/elevators').as('rotaElevator');

        cy.get('#mat-input-10').type('ELEV' + numeroAleatorio.toString());
        cy.get('#mat-select-value-7').click().get('#mat-option-5').click();
        cy.get('#mat-select-value-9').click().get('#mat-option-4').get('.mat-pseudo-checkbox').click({ force: true });
        //cy.get('#mat-option-4', { timeout: 10000 }).should('not.be.visible');
        cy.get('.cdk-overlay-backdrop').click();
        cy.get('#saveNewElevator').click();




        cy.wait('@rotaElevator').should(({ response }) => {
            expect(response!.statusCode).to.eq(200);
        });
    });

});
