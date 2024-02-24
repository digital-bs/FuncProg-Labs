mtype = {CUSTOMER_CAN_GO, QUEUE_IS_FREE, REQUEST_TICKET, RETURN_TICKET, REQUEST_CARD, RETURN_CARD, REQUEST_MONEY, RETURN_MONEY, OPERATOR_CALLING, NEW_CUSTOMER_FOR_OPERATOR};



chan queue_to_customer = [0] of {mtype};

chan customer_to_queue = [0] of {mtype};

chan customer_to_terminal = [0] of {mtype}; 

chan customer_to_operator = [0] of {mtype}; 

chan customer_to_bank = [0] of {mtype}; 

chan terminal_to_customer = [0] of {mtype};

chan operator_to_customer = [0] of {mtype};

chan terminal_to_operator = [0] of {mtype};

chan bank_to_customer = [0] of {mtype};

bool ok = true;



active proctype queue()

{

    mtype msg;

    do

        ::true ->

        {

            printf("customer can go to terminal\n");

            queue_to_customer ! CUSTOMER_CAN_GO;

            customer_to_queue ? msg;

            if

            :: msg == QUEUE_IS_FREE -> 

            {

                printf("now queue is free, next customer\n");

            }

            :: else -> ok = false;

            fi

        }



    od



}





active proctype customer()

{

    mtype msg;

    do

        :: true -> {

        queue_to_customer ? msg;

        if

            ::msg == CUSTOMER_CAN_GO ->

            {

                printf("customer gonna take ticket \n");

                customer_to_terminal ! REQUEST_TICKET;

                operator_to_customer ? msg;

                if

                    :: msg == OPERATOR_CALLING ->

                    {

                        printf("operator is calling\n");

                        customer_to_operator ! REQUEST_CARD;

                        operator_to_customer ? msg;

                        if

                            :: msg == RETURN_CARD ->

                            {

                                printf("card received\n");

                                printf("customer make request for money\n");

                                customer_to_bank ! REQUEST_MONEY;

                                bank_to_customer ? msg;

                                if

                                    :: msg == RETURN_MONEY ->

                                    {

                                        printf("customer received money and happy \n");

                                        customer_to_queue ! QUEUE_IS_FREE;

                                    }

                                    :: else -> ok = false;

                                fi

                            }

                            :: else -> ok = false;

                        fi

                    }

                    :: else -> ok = false;                

                fi

            }

            :: else -> ok = false;

        fi

        }

    od

}



active proctype terminal()

{

    mtype msg;

    printf("terminal is active\n");

    do

        :: true -> 

        {

            customer_to_terminal ? msg;

            if

            :: msg == REQUEST_TICKET -> 

            {

                printf("new customer checked\n");

                terminal_to_operator ! NEW_CUSTOMER_FOR_OPERATOR;

            }

            :: else -> ok = false;

            fi

        }

    od

    

}



active proctype operator()

{

    mtype msg;

    printf("operator is active\n");

    do

        :: true -> 

        {

            terminal_to_operator ? msg;

            if

            :: msg == NEW_CUSTOMER_FOR_OPERATOR -> 

            {

                printf("operator calling customer\n");

                operator_to_customer ! OPERATOR_CALLING;

                customer_to_operator ? msg;

                if

                :: msg == REQUEST_CARD -> 

                {

                    printf("operator get request for card\n");

                    operator_to_customer ! RETURN_CARD;

                }

                :: else -> ok = false;

                fi

            }

            :: else -> ok = false;

            fi

        }

    od    

}





active proctype bank()

{

    mtype msg;

    printf("bank is active\n");

    do

        :: true -> 

        {

            customer_to_bank ? msg;

            if

            :: msg == REQUEST_MONEY -> 

            {

                printf("money_request received\n");

                bank_to_customer ! RETURN_MONEY;

            }

            :: else -> ok = false;

            fi

        }

    od    

}


ltl allok {[] ok}